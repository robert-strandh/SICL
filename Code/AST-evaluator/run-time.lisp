(cl:in-package #:sicl-ast-evaluator)

(defun symbol-value (name global-environment)
  (loop for entry in sicl-run-time:*dynamic-environment*
        when (and (typep entry 'sicl-run-time:special-variable-entry)
                  (eq name (sicl-run-time:name entry)))
          return (sicl-run-time:value entry)
        finally (multiple-value-bind (boundp value)
                    (env:special-variable
                     (env:client global-environment) global-environment name)
                  (if boundp
                      (return value)
                      (error "unbound variable ~s" name)))))

(defun (setf symbol-value) (value name global-environment)
  (loop for entry in sicl-run-time:*dynamic-environment*
        when (and (typep entry 'sicl-run-time:special-variable-entry)
                  (eq name (sicl-run-time:name entry)))
          do (setf (sicl-run-time:value entry) value)
          and return value
        finally (setf (env:special-variable
                       (env:client global-environment) global-environment name t)
                      value)
                (return value)))

(defmacro with-variable-bound ((name-form value-form) &body body)
  `(let ((sicl-run-time:*dynamic-environment*
           (cons (make-instance 'sicl-run-time:special-variable-entry
                   :name ,name-form
                   :value ,value-form)
                 sicl-run-time:*dynamic-environment*)))
     ,@body))

(defmacro with-exit-point ((name) &body body)
  `(let ((sicl-run-time:*dynamic-environment*
           (cons (make-instance 'sicl-run-time:block/tagbody-entry
                   :stack-pointer nil
                   :frame-pointer nil
                   :identifier ',name)
                 sicl-run-time:*dynamic-environment*)))
     ,@body))

(defmacro with-unwind-protect ((thunk-form) &body body)
  `(let ((sicl-run-time:*dynamic-environment*
           (cons (make-instance 'sicl-run-time:unwind-protect-entry
                   :thunk ,thunk-form)
                 sicl-run-time:*dynamic-environment*)))
     ,@body))

(defun unwind (name)
  (let ((sicl-run-time:block/tagbody-entry
          (loop for entry in sicl-run-time:*dynamic-environment*
                when (and (typep entry 'sicl-run-time:block/tagbody-entry)
                          (eq (sicl-run-time:identifier entry) name))
                  return entry)))
    (when (null sicl-run-time:block/tagbody-entry)
      (error 'attempt-to-exit-to-an-invalid-exit-point))
    (loop until (eq sicl-run-time:block/tagbody-entry
                    (first sicl-run-time:*dynamic-environment*))
          do (let ((entry (pop sicl-run-time:*dynamic-environment*)))
               (when (typep entry 'sicl-run-time:unwind-protect-entry)
                 (funcall (sicl-run-time:thunk entry)))))))
