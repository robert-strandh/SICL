(cl:in-package #:sicl-clos)

;;; This function is called by ADD-DIRECT-METHOD and
;;; REMOVE-DIRECT-METHOD so change the list of generic functions
;;; having SPECIALIZER as a specializer.
(defgeneric (setf specializer-direct-generic-functions)
    (new-generic-functions specializer))
