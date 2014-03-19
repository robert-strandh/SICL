(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_no_nex.htm#no-next-method
(defgeneric no-next-method (generic-function method &rest args))
