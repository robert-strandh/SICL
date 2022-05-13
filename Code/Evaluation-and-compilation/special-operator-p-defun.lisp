(cl:in-package #:sicl-evaluation-and-compilation)

(symbol-macrolet ((c sicl-client:*client*))
  (let ((e env:*environment*))
    (defun special-operator-p (symbol)
      (env:special-operator-p c e symbol))))
