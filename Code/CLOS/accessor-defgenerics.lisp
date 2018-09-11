(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/specializer-direct-generic-functions.html
(defgeneric specializer-direct-generic-functions (specializer))

;;; This function is called by ADD-DIRECT-METHOD and
;;; REMOVE-DIRECT-METHOD so change the list of generic functions
;;; having SPECIALIZER as a specializer.
(defgeneric (setf specializer-direct-generic-functions)
    (new-generic-functions specializer))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/specializer-direct-methods.html
(defgeneric specializer-direct-methods (specializer))

;;; This function is called by ADD-DIRECT-METHOD and
;;; REMOVE-DIRECT-METHOD so change the list of methods having
;;; SPECIALIZER as a specializer.
(defgeneric (setf specializer-direct-methods) (new-methods specializer))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/eql-specializer-object.html
(defgeneric eql-specializer-object (eql-specializer))

;;; This function returns the unique number of the class, assigned
;;; when the class is initialized or reinitialized.
(defgeneric unique-number (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-name.html
(defgeneric class-name (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-direct-subclasses.html
(defgeneric class-direct-subclasses (class))

;;; The functions ADD-DIRECT-SUBCLASS and REMOVE-DIRECT-SUBCLASS are
;;; used to update the direct subclasses of a class, so they call this
;;; function.
(defgeneric (setf class-direct-subclasses) (direct-subclassees class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-direct-default-initargs.html
(defgeneric class-direct-default-initargs (class))

;;; This generic function is used to access the documentation slot of
;;; all metaobjects that have such a slot.  This reader is used by
;;; certain methods on CL:DOCUMENTATION.
(defgeneric documentation (metaobject))

;;; This generic function is used to write the documentation slot of
;;; all metaobjects that have such a slot.
(defgeneric (setf documentation) (new-documentation metaobject))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-finalized-p.html
(defgeneric class-finalized-p (class))

;;; This function is used by the class finalization protocol to set
;;; the flag in the class that indicates that it is finalized.
(defgeneric (setf class-finalized-p) (new-value class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-precedence-list.html
(defgeneric class-precedence-list (class))

;;; This function is similar to CLASS-PRECEDENCE-LIST, except that
;;; CLASS-PRECEDENCE-LIST is specified to signal an error when the
;;; class is not finalized, which we accomplish by using a :BEFORE
;;; method.  However, during class finalization, we need to access the
;;; class precedence list in the two steps following its computation.
;;; Since at that point the class is not yet finalized, those two
;;; steps can not call CLASS-PRECEDENCE-LIST.  Our solution is to
;;; define an alternative reader for the same slot, named
;;; PRECEDENCE-LIST, and which does not signal an error if the class
;;; is not finalized.
(defgeneric precedence-list (class))

;;; This function is used by the class finalization protocol to set
;;; the precedence list of the class.
(defgeneric (setf precedence-list) (precedence-list class))

;;; This function is used by ALLOCATE-INSTANCE and
;;; ALLOCATE-BUILT-IN-INSTANCE to determine the size of the instance
;;; to allocate.
(defgeneric instance-size (class))

;;; This function is used during class finalization, when the
;;; effective slots are known, and it is therefore also known what
;;; size the instances of this class should have.
(defgeneric (setf instance-size) (new-size class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-direct-slots.html
(defgeneric class-direct-slots (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-direct-superclasses.html
(defgeneric class-direct-superclasses (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-default-initargs.html
(defgeneric class-default-initargs (class))

;;; This function is used by the class finalization protocol to set
;;; the default initargs of the class. 
(defgeneric (setf class-default-initargs) (default-initargs class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-slots.html
(defgeneric class-slots (class))

;;; This function is used by the class finalization protocol to set
;;; the effective slots of the class. 
(defgeneric (setf class-slots) (effective-slots class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-prototype.html
(defgeneric class-prototype (class))

;;; This function sets the class prototype of the class.
(defgeneric (setf class-prototype) (prototype class))

;;; This function returns a list of the dependents of the metaobject.
(defgeneric dependents (metaobject))

(defgeneric (setf dependents) (dependents class))

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

;;; This function sets the methods of the generic function.
(defgeneric (setf generic-function-methods) (new-methods generic-function))

;;; This generic function returns a list of the initial methods of the
;;; generic function.  Recall that the initial methods of a generic
;;; function are those that are defined as part of the evaluation of
;;; the DEFGENERIC form.
(defgeneric initial-methods (generic-function))

;;; This function sets the initial methods of the generic function.
;;; Recall that the initial methods of a generic function are those
;;; that are defined as part of the evaluation of the DEFGENERIC form.
(defgeneric (setf initial-methods) (new-methods generic-function))

;;; This function returns the call history of the generic function.
;;; FIXME: say more.
(defgeneric call-history (generic-function))

;;; This function sets the call history of the generic function.
;;; FIXME: say more.
(defgeneric (setf call-history) (new-call-history generic-function))

;;; This function returns the specializer profile of the generic function.
;;; FIXME: say more.
(defgeneric specializer-profile (generic-function))

;;; This function sets the specializer profile of the generic function.
;;; FIXME: say more.
(defgeneric (setf specializer-profile) (new-specializer-profile generic-function))

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

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-specializers.html
(defgeneric method-specializers (method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/method-qualifiers.html
(defgeneric method-qualifiers (method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/accessor-method-slot-definition.html
(defgeneric accessor-method-slot-definition (accessor-method))

(defgeneric (setf accessor-method-slot-definition)
    (slot-definition accessor-method))

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

;;; For a slot with :ALLOCATION :CLASS, this function returns a CONS
;;; cell where the CAR is used to store the value of the slot.
(defgeneric slot-definition-storage (direct-slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-readers.html
(defgeneric slot-definition-readers (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-writers.html
(defgeneric slot-definition-writers (slot-definition))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-definition-location.html
(defgeneric slot-definition-location (slot-definition))

(defgeneric (setf slot-definition-location) (new-location slot-definition))

(defgeneric operation (simple-method-combination))

(defgeneric variant-signature (method-combination))

(defgeneric template (method-combination))

(defgeneric code-object (method-combination))

;;  LocalWords:  DEFGENERIC
