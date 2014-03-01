(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_no_app.htm#no-applicable-method
(defgeneric no-applicable-method (generic-function &rest function-arguments))
