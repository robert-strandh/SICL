(cl:in-package #:sicl-clos)

(defgeneric no-applicable-method (generic-function &rest function-arguments))

(defmethod no-applicable-method (generic-function &rest function-arguments)
  (error "no applicable method on generic function ~s with arguments: ~s"
	 generic-function function-arguments))

