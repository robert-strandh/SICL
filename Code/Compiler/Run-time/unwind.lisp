(cl:in-package #:sicl-run-time)

(defun unwind (continuation)
  (let ((dynamic-environment (sicl-primop:dynamic-environment)))
    ;; Start by finding the valid BLOCK/TAGBODY-ENTRY with the right
    ;; continuation.
    (let ((exit-point (loop for entry in dynamic-environment
                            when (and (typep entry 'block/tagbody-entry)
                                      (eq continuation (continuation entry)))
                              do (if (validp entry)
                                     (return entry)
                                     (error "Attempt to unwind to an abandoned entry"))
                            finally (error "Attempt to unwind to a non-existing entry"))))
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
      ;; Set the stack frame
      (sicl-primop:establish-stack-frame
       (stack-pointer exit-point)
       (frame-pointer exit-point))
      ;; Return to the instruction that will jump to the right
      ;; address.
      (values))))
