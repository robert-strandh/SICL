(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_no_app.htm#no-applicable-method
(defgeneric no-applicable-method (generic-function &rest function-arguments))

(defmethod no-applicable-method
    ((generic-function generic-function) &rest function-arguments)
  (error "no applicable method on generic function ~s with arguments: ~s"
         generic-function function-arguments))
