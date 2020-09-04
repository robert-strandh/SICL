(cl:in-package #:sicl-ast-evaluator)

(defclass special-variable-entry ()
  ((%name :initarg :name :reader name)
   (%value :initarg :value :accessor value)))

(defun symbol-value (name global-environment)
  (loop for entry in sicl-run-time:*dynamic-environment*
        when (and (typep entry 'special-variable-entry)
                  (eq name (name entry)))
          return (value entry)
        ;; FIXME: make sure it is bound in the global environment
        finally (return (env:special-variable
                         (env:client global-environment) global-environment name))))

(defun (setf symbol-value) (value name global-environment)
  (loop for entry in sicl-run-time:*dynamic-environment*
        when (and (typep entry 'special-variable-entry)
                  (eq name (name entry)))
          do (setf (value entry) value)
          and return value
        finally (setf (env:special-variable
                       (env:client global-environment) global-environment name t)
                      value)
                (return value)))

(defmacro with-variable-bound ((name-form value-form) &body body)
  `(let ((sicl-run-time:*dynamic-environment*
           (cons (make-instance 'special-variable-entry
                   :name ,name-form
                   :value ,value-form)
                 sicl-run-time:*dynamic-environment*)))
     ,@body))

(defclass exit-point-entry ()
  ((%name :initarg :name :reader name)
   (%validp :initform t :accessor validp)))

(defmacro with-exit-point ((name) &body body)
  `(let ((sicl-run-time:*dynamic-environment*
           (cons (make-instance 'exit-point-entry
                   :name ',name)
                 sicl-run-time:*dynamic-environment*)))
     ,@body))

(defclass unwind-protect-entry ()
  ((%thunk :initarg :thunk :reader thunk)))

(defmacro with-unwind-protect ((thunk-form) &body body)
  `(let ((sicl-run-time:*dynamic-environment*
           (cons (make-instance unwind-protect-entry
                   :thunk ,thunk-form)
                 sicl-run-time:*dynamic-environment*)))
     ,@body))

(defun unwind (name)
  (let ((exit-point-entry (loop for entry in sicl-run-time:*dynamic-environment*
                                when (and (typep entry 'exit-point-entry)
                                          (eq (name entry) name))
                                  return entry)))
    (when (null exit-point-entry)
      (error 'attempt-to-exit-to-an-invalid-exit-point))
    (loop until (eq exit-point-entry (first sicl-run-time:*dynamic-environment*))
          do (let ((entry (pop sicl-run-time:*dynamic-environment*)))
               (when (typep entry 'unwind-protect-entry)
                 (funcall (thunk entry)))))))

(defparameter *call-stack* '())

(defclass invocation ()
  ((%location :initarg :location :reader location)
   (%arguments :initarg :arguments :reader arguments)))
