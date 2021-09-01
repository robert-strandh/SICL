(cl:in-package #:sicl-run-time)

(defun throw (tag values)
  (let ((dynamic-environment (sicl-primop:dynamic-environment)))
    ;; Start by finding the valid CATCH-ENTRY with the right tag.
    (let ((exit-point (loop for entry in dynamic-environment
                            when (and (typep entry 'catch-entry)
                                      (eq tag (tag entry)))
                              do (if (validp entry)
                                     (return entry)
                                     (error "Attempt to throw to an abandoned entry"))
                            finally (error "Attempt to throw to a non-existing entry"))))
      ;; Next, abandon any intermediate exit points.
      (loop for entry in dynamic-environment
            until (eq entry exit-point)
            when (typep entry 'exit-point)
              do (setf (validp entry) nil))
      ;; Next, execute thunks of UNWIND-PROTECT entries.
      (loop for env = dynamic-environment then (rest env)
            for entry = (first env)
            until (eq entry exit-point)
            when (typep entry 'unwind-protect-entry)
              do (sicl-primop:with-dynamic-environment
                     (rest env)
                   (funcall (thunk entry))))
      ;; Call the throw function.
      (funcall (throw-function exit-point) values))))
