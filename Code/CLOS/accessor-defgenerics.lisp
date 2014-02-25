(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors from the AMOP.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readers for specializer metaobjects other than classes.

(defgeneric specializer-direct-generic-functions (specializer))

(defgeneric specializer-direct-methods (specializer))

(defgeneric eql-specializer-object (eql-specializer))

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

;;; During phase 3 of bootstrapping, the generic function
;;; GENERIC-FUNCTION-METHODS is used to compute wich methods are
;;; applicable on a bridge generic function, so that the entire
;;; mechanism for executing generic functions can work.  However, at
;;; the same time, methods are added to target generic functions, so
;;; there must be a way to access the methods of a target generic
;;; function as well.  It might have been possible to convince the
;;; bridge generic function to do both, but we didn't do that.
;;; Instead, we have a second reader for the slot containing the
;;; methods of a generic function which is used for target generic
;;; functions.
(defgeneric gf-methods (generic-function))

;;; The functions ADD-DIRECT-SUBCLASS and REMOVE-DIRECT-SUBCLASS are
;;; used to update the direct subclasses of a class, so they call this
;;; function.
(defgeneric (setf c-direct-subclasses) (new-value class))

;;; This function will be removed later.  For now it plays the role of
;;; the function CL:DOCUMENTATION. 
(defgeneric gf-documentation (generic-function))

;;; This function returns a list of the dependents of the metaobject.
(defgeneric dependents (metaobject))

;;; This function sets the list dependents of the metaobject.
(defgeneric (setf dependents) (new-dependents metaobject))

;;; This function sets the methods of the generic function.
(defgeneric (setf gf-methods) (new-methods generic-function))

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

;;; This function returns the cache of the generic function.
;;; FIXME: say more.
(defgeneric set-cache (generic-function))

;;; This function sets the cache of the generic function.
;;; FIXME: say more.
(defgeneric (setf set-cache) (new-set-cache generic-function))

;;; This function is called by ADD-METHOD and REMOVE-METHOD to assign
;;; the generic function to which the method is associated.
(defgeneric (setf m-generic-function) (generic-function method))

;;; This function will be removed later.  For now it plays the role of
;;; the function CL:DOCUMENTATION. 
(defgeneric method-documentation (generic-function))

;;; This function will be removed later.  For now it plays the role of
;;; the function (SETF CL:DOCUMENTATION). 
(defgeneric (setf method-documentation) (documentation generic-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors for specializer metaobjects other than classes.

;;; This function is called by ADD-DIRECT-METHOD and
;;; REMOVE-DIRECT-METHOD so change the list of generic functions
;;; having SPECIALIZER as a specializer.
(defgeneric (setf s-direct-generic-functions) (new-generic-functions specializer))

;;; This function is called by ADD-DIRECT-METHOD and
;;; REMOVE-DIRECT-METHOD so change the list of methods having
;;; SPECIALIZER as a specializer.
(defgeneric (setf s-direct-methods) (new-methods specializer))

;;; This function returns the unique number of the class, assigned
;;; when the class is initialized or reinitialized.
(defgeneric unique-number (class))

;;; This function sets the direct subclasses of the class.
(defgeneric (setf c-direct-subclasses) (direct-subclasses class))

;;; This function sets the direct methods of the class.
(defgeneric (setf c-direct-methods) (direct-methods class))

(defgeneric default-initargs (class))

;;; For STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS, this function
;;; returns the same value as CLASS-DIRECT-DEFAULT-INITARGS.  However,
;;; whereas for BUILT-IN-CLASS CLASS-DIRECT-DEFAULT-INITARGS must
;;; return the empty list according to the MOP, this function returns
;;; the direct default initargs of the built-in class.
(defgeneric direct-default-initargs (class))

;;; For STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS, this function
;;; returns the same value as CLASS-DIRECT-SLOTS.  However, whereas
;;; for BUILT-IN-CLASS CLASS-DIRECT-SLOTS must return the empty list
;;; according to the MOP, this function returns the direct default
;;; slots of the built-in class.
(defgeneric direct-slots (class))

;;; This function will be removed later.  For now it plays the role of
;;; the function CL:DOCUMENTATION. 
(defgeneric c-documentation (generic-function))

;;; This function will be removed later.  For now it plays the role of
;;; the function (SETF CL:DOCUMENTATION). 
(defgeneric (setf c-documentation) (documentation generic-function))

;;; This function is used by the class finalization protocol to set
;;; the flag in the class that indicates that it is finalized.
(defgeneric (setf c-finalized-p) (new-value class))

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

;;; This function is used by the class finalization protocol to set
;;; the default initargs of the class. 
(defgeneric (setf c-default-initargs) (default-initargs class))

;;; For STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS, this function
;;; returns the same value as CLASS-SLOTS.  However, whereas for
;;; BUILT-IN-CLASS CLASS-SLOTS must return the empty list according to
;;; the MOP, this function returns the effective default slots of the
;;; built-in class.
(defgeneric effective-slots (class))

;;; This function is used by the class finalization protocol to set
;;; the effective slots of the class. 
(defgeneric (setf c-slots) (effective-slots class))

;;; This function sets the class prototype of the class.
(defgeneric (setf c-prototype) (prototype class))

;;; This function is used by ALLOCATE-INSTANCE and
;;; ALLOCATE-BUILT-IN-INSTANCE to determine the size of the instance
;;; to allocate.
(defgeneric instance-size (class))

;;; This function is used during class finalization, when the
;;; effective slots are known, and it is therefore also known what
;;; size the instances of this class should have.
(defgeneric (setf instance-size) (new-size class))

;;; This function returns the dependents of the class.
(defgeneric dependents (class))

;;; This function sets the dependents of the class.
(defgeneric (setf dependents) (dependents class))

;;; For a slot with :ALLOCATION :CLASS, this function returns a CONS
;;; cell where the CAR is used to store the value of the slot.
(defgeneric slot-definition-storage (direct-slot-definition))

(defgeneric (setf slot-definition) (slot-definition accessor-method))

(defgeneric (setf s-location) (location slot-definition))

(defgeneric symbol-name (symbol))

(defgeneric symbol-package (symbol))
