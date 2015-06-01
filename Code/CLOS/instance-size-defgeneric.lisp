(cl:in-package #:sicl-clos)

;;; This function is used by ALLOCATE-INSTANCE and
;;; ALLOCATE-BUILT-IN-INSTANCE to determine the size of the instance
;;; to allocate.
(defgeneric instance-size (class))
