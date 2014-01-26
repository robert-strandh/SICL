(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors from the AMOP.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readers for class metaobjects.
;;;
;;; For a list of specified readers of these metaobjects, see
;;; http://metamodular.com/CLOS-MOP/readers-for-class-metaobjects.html

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-default-initargs.html
(defgeneric class-default-initargs (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-direct-default-initargs.html
(defgeneric class-direct-default-initargs (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-name.html
(defgeneric class-name (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-direct-superclasses.html
(defgeneric class-direct-superclasses (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-direct-slots.html
(defgeneric class-direct-slots (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-direct-subclasses.html
(defgeneric class-direct-subclasses (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-slots.html
(defgeneric class-slots (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-precedence-list.html
(defgeneric class-precedence-list (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-finalized-p.html
(defgeneric class-finalized-p (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-prototype.html
(defgeneric class-prototype (class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readers for generic function metaobjects.
;;;
;;; For a list of specified readers of these metaobjects, see
;;; see
;;; http://metamodular.com/CLOS-MOP/readers-for-generic-function-metaobjects.html

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-name.html
(defgeneric generic-function-name (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-lambda-list.html
(defgeneric generic-function-lambda-list (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-argument-precedence-order.html
(defgeneric generic-function-argument-precedence-order (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-declarations.html
(defgeneric generic-function-declarations (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-method-class.html
(defgeneric generic-function-method-class (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-method-combination.html
(defgeneric generic-function-method-combination (generic-function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/generic-function-methods.html
(defgeneric generic-function-methods (generic-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readers for method metaobjects.
;;;
;;; For a list of specified readers of these metaobjects, see
;;; http://metamodular.com/CLOS-MOP/readers-for-method-metaobjects.html

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-function.html
(defgeneric method-function (method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-generic-function.html
(defgeneric method-generic-function (method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-lambda-list.html
(defgeneric method-lambda-list (method))


;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-specializers.html
(defgeneric method-specializers (method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-qualifiers.html
(defgeneric method-qualifiers (method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/accessor-method-slot-definition.html
(defgeneric accessor-method-slot-definition (accessor-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readers for slot definition metaobjects.
;;;
;;; For a list of specified readers of these metaobjects, see
;;; http://metamodular.com/CLOS-MOP/readers-for-slot-definition-metaobjects.html

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-allocation.html
(defgeneric slot-definition-allocation (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-initargs.html
(defgeneric slot-definition-initargs (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-initform.html
(defgeneric slot-definition-initform (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-initfunction.html
(defgeneric slot-definition-initfunction (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-name.html
(defgeneric slot-definition-name (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-type.html
(defgeneric slot-definition-type (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-readers.html
(defgeneric slot-definition-readers (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-writers.html
(defgeneric slot-definition-writers (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-location.html
(defgeneric slot-definition-location (slot-definition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SICL-specific accessors.

;;; The functions ADD-DIRECT-SUBCLASS and REMOVE-DIRECT-SUBCLASS are
;;; used to update the direct subclasses of a class, so they call this
;;; function.
(defgeneric (setf c-direct-subclasses) (new-value class))

(defgeneric standard-instance-slots (standard-instance))
(defgeneric standard-instance-timestamp (standard-instance))
(defgeneric (setf gf-lambda-list) (new-lambda-list generic-function))
(defgeneric gf-documentation (generic-function))
(defgeneric (setf gf-documentation) (generic-function))
(defgeneric discriminating-function (generic-function))
(defgeneric (setf discriminating-function)
    (new-discriminating-function generic-function))
(defgeneric dependents (generic-function))
(defgeneric (setf dependents) (new-dependents generic-function))
(defgeneric (setf gf-argument-precedence-order)
    (new-argument-precedence-order generic-function))
(defgeneric (setf gf-declarations) (new-declarations generic-function))
(defgeneric (setf gf-method-class) (new-method-class generic-function))
(defgeneric (setf gf-method-combination) (new-method-combination generic-function))
(defgeneric (setf gf-methods) (new-methods generic-function))
(defgeneric call-history (generic-function))
(defgeneric (setf call-history) (new-call-history generic-function))
(defgeneric specializer-profile (generic-function))
(defgeneric (setf specializer-profile) (new-specializer-profile generic-function))
(defgeneric set-cache (generic-function))
(defgeneric (setf set-cache) (new-set-cache generic-function))

