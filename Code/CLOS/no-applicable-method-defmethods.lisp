(cl:in-package #:sicl-clos)

(defmethod no-applicable-method
    ((generic-function generic-function) &rest function-arguments)
  (error "no applicable method on generic function ~s with arguments: ~s"
         generic-function function-arguments))
