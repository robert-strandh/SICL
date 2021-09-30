(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-name.html
(defgeneric slot-definition-name (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-allocation.html
(defgeneric slot-definition-allocation (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-type.html
(defgeneric slot-definition-type (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-initargs.html
(defgeneric slot-definition-initargs (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-initform.html
(defgeneric slot-definition-initform (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-initfunction.html
(defgeneric slot-definition-initfunction (slot-definition))

;;; This slot reader is not required by the specification,
;;; but we define it anyway, so that it can be called by 
;;; the standard function DOCUMENTATION.
(defgeneric slot-definition-documentation (slot-definition))

(defclass slot-definition (metaobject)
  ((%name 
    :initarg :name
    :reader slot-definition-name)
   (%allocation 
    :initarg :allocation
    :initform :instance
    :reader slot-definition-allocation)
   (%type 
    :initarg :type
    :initform t
    :reader slot-definition-type)
   (%initargs 
    :initform '()
    :initarg :initargs 
    :reader slot-definition-initargs)
   (%initform 
    :initarg :initform 
    :initform nil
    :reader slot-definition-initform)
   (%initfunction
    :initform nil
    :initarg :initfunction
    :reader slot-definition-initfunction)
   (%documentation
    :initform nil
    :initarg :documentation
    :reader slot-definition-documentation)))
