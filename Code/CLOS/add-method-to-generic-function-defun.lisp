(cl:in-package #:sicl-clos)

;;; This function is a simple indirection for ADD-METHOD.  It is used
;;; during bootstrapping.
(defun add-method-to-generic-function (generic-function method)
  (add-method generic-function method))
