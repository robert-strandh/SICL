(cl:in-package #:sicl-clos)

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
