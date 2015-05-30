(cl:in-package #:sicl-clos)

;;; This function is called by ADD-DIRECT-METHOD and
;;; REMOVE-DIRECT-METHOD so change the list of methods having
;;; SPECIALIZER as a specializer.
(defgeneric (setf specializer-direct-methods) (new-methods specializer))
