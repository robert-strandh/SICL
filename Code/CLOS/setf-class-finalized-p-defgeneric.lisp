(cl:in-package #:sicl-clos)

;;; This function is used by the class finalization protocol to set
;;; the flag in the class that indicates that it is finalized.
(defgeneric (setf class-finalized-p) (new-value class))
