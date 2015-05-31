(cl:in-package #:sicl-clos)

;;; This function is called by ADD-METHOD and REMOVE-METHOD to assign
;;; the generic function to which the method is associated.
(defgeneric (setf method-generic-function) (generic-function method))
