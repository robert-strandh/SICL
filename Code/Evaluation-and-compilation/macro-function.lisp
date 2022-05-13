(cl:in-package #:sicl-evaluation-and-compilation)

(let ((e env:*environment*))
  (defun macro-function (symbol &optional (environment e))
    (trucler:macro-function symbol e)))

(symbol-macrolet ((c sicl-client:*client*))
  (let ((e env:*environment*))
    (defun (setf macro-function)
        (new-function symbol &optional environment)
      (assert (null environment))
      (setf (env:fdefinition c e symbol) nil)
      (setf (env:macro-function c e symbol new-function))
      new-function)))
