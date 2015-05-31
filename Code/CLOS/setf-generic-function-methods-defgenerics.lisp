(cl:in-package #:sicl-clos)

;;; This function sets the methods of the generic function.
(defgeneric (setf generic-function-methods) (new-methods generic-function))
