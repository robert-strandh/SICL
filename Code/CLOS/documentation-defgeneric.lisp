(cl:in-package #:sicl-clos)

;;; This generic function is used to access the documentation slot of
;;; all metaobjects that have such a slot.  This reader is used by
;;; certain methods on CL:DOCUMENTATION.
(defgeneric documentation (metaobject))

;;  LocalWords:  metaobjects defgeneric
