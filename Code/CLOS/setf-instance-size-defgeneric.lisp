(cl:in-package #:sicl-clos)

;;; This function is used during class finalization, when the
;;; effective slots are known, and it is therefore also known what
;;; size the instances of this class should have.
(defgeneric (setf instance-size) (new-size class))
