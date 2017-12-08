(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/ensure-generic-function-using-class.html
;;;
;;; We handle an additional keyword argument compared to the
;;; specification, namely ENVIRONMENT.  It can be used to specify in
;;; which the generic function is to be found or defined.  It defaults
;;; to the environment in which this function was loaded.
(defgeneric ensure-generic-function-using-class
    (generic-function
     function-name
     &key
       argument-precedence-order
       declarations
       documentation
       generic-function-class
       lambda-list
       method-class
       method-combination
       name
       environment
     &allow-other-keys))
