(cl:in-package #:sicl-clos)

;;; This function is used by the class finalization protocol to set
;;; the precedence list of the class.
(defgeneric (setf precedence-list) (precedence-list class))
