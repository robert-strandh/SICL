(cl:in-package #:sicl-clos)

;;; This generic function is used to write the documentation slot of
;;; all metaobjects that have such a slot.
(defgeneric (setf documentation) (documentation metaobject))

;;  LocalWords:  metaobjects defgeneric
