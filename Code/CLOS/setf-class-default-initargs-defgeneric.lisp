(cl:in-package #:sicl-clos)

;;; This function is used by the class finalization protocol to set
;;; the default initargs of the class. 
(defgeneric (setf class-default-initargs) (default-initargs class))
