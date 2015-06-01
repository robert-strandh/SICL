(cl:in-package #:sicl-clos)

;;; This function is used by the class finalization protocol to set
;;; the effective slots of the class. 
(defgeneric (setf class-slots) (effective-slots class))
