(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-applicable-methods.html
;;;
;;; This generic function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_comput.htm#compute-applicable-methods
(defgeneric compute-applicable-methods (generic-function arguments))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-applicable-methods-using-classes.html
(defgeneric compute-applicable-methods-using-classes
    (generic-function classes-of-arguments))
