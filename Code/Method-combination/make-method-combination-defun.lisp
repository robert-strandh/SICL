(cl:in-package #:sicl-method-combination)

(defun make-method-combination (variant-signature template)
  (make-instance 'method-combination
    :variant-signature variant-signature
    :template template))
