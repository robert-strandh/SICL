(cl:in-package #:sicl-run-time)

(defun unwind (continuation)
  (let ((dynamic-environment (sicl-primop:dynamic-environment)))
    ;; Start by finding the valid BLOCK/TAGBODY-ENTRY with the right
    ;; continuation.
    (let ((exit-point (loop for entry in dynamic-environment
                            when (and (typep entry 'block/tagbody-entry)
                                      (eq continuation (continuation entry)))
                              do (if (valid-p entry)
                                     (return entry)
                                     (error "Attempt to unwind to an abandoned entry"))
                            finally (error "Attempt to unwind to a non-existing entry"))))
      ;; Next, abandon any intermediate exit points.
      (loop for entry in dynamic-environment
            until (eq entry exit-point)
            when (typep entry 'exit-point)
              do (setf (valid-p entry) nil))
      ;; Next, execute thunks of UNWIND-PROTECT entries.  FIXME: this
      ;; is not quite correct.  We need to find a way to execute
      ;; UNWIND-PROTECT thunks in the partially unwound dynamic
      ;; environment.
      (loop for entry in dynamic-environment
            until (eq entry exit-point)
            when (typep entry 'unwind-protect-entry)
              ;; This is where we somehow need to modify the dynamic
              ;; environment.
              do (funcall (thunk entry)))
      ;; Set the stack frame
      (sicl-primop:establish-stack-frame
       (stack-pointer exit-point)
       (frame-pointer exit-point))
      ;; Return to the instruction that will jump to the right
      ;; address.
      (values))))
