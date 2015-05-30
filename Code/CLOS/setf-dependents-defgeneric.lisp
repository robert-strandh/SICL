(cl:in-package #:sicl-clos)

;;; This function sets the list dependents of the metaobject.
(defgeneric (setf dependents) (new-dependents metaobject))
