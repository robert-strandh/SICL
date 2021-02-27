(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-function.html
(defgeneric method-function (method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-generic-function.html
(defgeneric method-generic-function (method))

;;; This function is called by ADD-METHOD and REMOVE-METHOD to assign
;;; the generic function to which the method is associated.
(defgeneric (setf method-generic-function) (generic-function method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-lambda-list.html
(defgeneric method-lambda-list (method))

(defclass method (metaobject)
  ((%function 
    :initarg :function 
    :reader method-function)
   (%generic-function 
    :initform nil
    :initarg :generic-function
    :accessor method-generic-function)
   (%lambda-list 
    :initarg :lambda-list 
    :reader method-lambda-list)
   (%specializers 
    :initarg :specializers 
    :reader method-specializers)
   (%qualifiers 
    :initarg :qualifiers 
    :reader method-qualifiers)
   ;; This slot is not mentioned in the section "Readers for Method
   ;; Metaobjects" in the AMOP.  However, it is mentioned in the
   ;; section "Initialization of Method Metaobjects", so we include it
   ;; here.
   (%documentation 
    :initform nil
    :initarg :documentation
    :accessor documentation)))
