(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Metaobject hierarchy.

;;; The Metaobject hierarchy according to the AMOP table 5.1.
;;;
;;; Metaobject class                   Direct superclasses
;;; ----------------                   -------------------
;;; standard-object                    (t)
;;; funcallable-standard-object        (standard-object function)
;;; metaobject                         (standard-object)
;;; generic-function                   (metaobject
;;;                                     funcallable-standard-object)
;;; standard-generic-function          (generic-function)
;;; method                             (metaobject)
;;; standard-method                    (method)
;;; standard-accesssor-method          (standard-method)
;;; standard-reader-method             (standard-accesssor-method)
;;; standard-writer-method             (standard-accesssor-method)
;;; method-combination                 (metaobject)
;;; slot-definition                    (metaobject)
;;; direct-slot-definition             (slot-definition)
;;; effective-slot-definition          (slot-definition)
;;; standard-slot-definition           (slot-definition)
;;; standard-direct-slot-definition    (standard-slot-definition 
;;;                                     direct-slot-definition)
;;; standard-effective-slot-definition (standard-slot-definition 
;;;                                     effective-slot-definition)
;;; specializer                        (metaobject)
;;; eql-specializer                    (specializer)
;;; class                              (specializer)
;;; built-in-class                     (class)
;;; forward-referenced-class           (class)
;;; standard-class                     (class)
;;; funcallable-standard-class         (class)
;;; 
;;; The class t is an instance of built-in-class.
;;;
;;; The classes generic-function and standard-generic-function
;;; are instances of funcallable-standard-class.
;;;
;;; All other classes are instances of standard-class.

;;; For performance reasons, it is important to order the class
;;; definitions in this file in depth-first pre-order.  During
;;; bootstrapping, this is the order in which the classes will be
;;; finalized, and finalization is what assigns unique class numbers.
;;; The depth-first pre-order ensures that the classes in a subtree
;;; have consecutive class numbers, and during generic function
;;; dispatch, the implications are that membership in an entire
;;; subtree can be tested with at most 2 tests.  This property makes
;;; it cheap to define common superclasses and methods specialized to
;;; such classes.  For classes and generic functions defined at the
;;; application level, this additional performance may not make much
;;; difference, but for system-level classes and generic functions, it
;;; may significantly improve overall performance.

(defparameter *class-unique-number* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The MOP class hierarchy. 

(defconstant +class-unique-number-offset+ 0)
(defconstant +instance-slots-offset+ 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class T.

(define-built-in-class t () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCTION.

(define-built-in-class function (t)
  ((%entry-point :initarg :entry-point)
   (%linkage-rack :initarg :linkage-rack)
   (%environment :initform nil :initarg :environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-OBJECT.

(defclass standard-object (t) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCALLABLE-STANDARD-OBJECT.

(defclass funcallable-standard-object (standard-object function)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class METAOBJECT.

(defclass metaobject (standard-object)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes GENERIC-FUNCTION and STANDARD-GENERIC-FUNCTION.
;;;
;;; FIXME: I can not remember why I decided not to use initargs for
;;; the slots here, and instead calling explicit writers in :AFTER
;;; methods on INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE.

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

(defclass generic-function (metaobject funcallable-standard-object)
  (;; While there is a function named (SETF GENERIC-FUNCTION-NAME), it
   ;; is not a writer function in that it works by calling
   ;; REINITIALIZE-INSTANCE.
   (%name 
    :initform nil
    :initarg :name
    :reader generic-function-name)
   (%lambda-list 
    :initarg :lambda-list
    :reader generic-function-lambda-list)
   (%documentation 
    :initarg :documentation
    :initform nil
    :reader gf-documentation)
   (%dependents
    :initform '()
    :accessor dependents))
  (:metaclass funcallable-standard-class))

(defclass standard-generic-function (generic-function)
  ((%argument-precedence-order
    :initarg :argument-precedence-order
    :reader generic-function-argument-precedence-order)
   (%declarations 
    :initarg :declarations
    :reader generic-function-declarations)
   (%method-class 
    :initarg :method-class
    :reader generic-function-method-class)
   (%method-combination 
    :initarg :method-combination
    ;; FIXME: remove this later.
    :initform nil
    :reader generic-function-method-combination)
   (%methods 
    :initform '() 
    ;; This reader is the one that the AMOP specifies.
    :reader generic-function-methods
    :writer (setf methods))
   ;; We maintain a CALL HISTORY of the generic function.  This call
   ;; history is a list of call records.  Whenever a call is made to
   ;; the generic function with some call profile that has not yet
   ;; been used in a call, we compute the effective method to use, and
   ;; we add a call record to the call history.
   (%call-history 
    :initform '() 
    :accessor call-history)
   ;; This slot is set by ADD-METHOD and REMOVE-METHOD.
   ;; It cnotains a list that has the same length as the 
   ;; number of required parameters of the generic function.
   ;; It contains NIL in each position where only the class T
   ;; is specialized for, and T in each position where some
   ;; method specialized for something other than the class T.
   (%specializer-profile
    :initarg :specializer-profile
    :accessor specializer-profile))
  (:metaclass funcallable-standard-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class METHOD

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

(defclass method (metaobject)
  ((%function 
    :initarg :function 
    :reader method-function)
   (%generic-function 
    :initform nil
    :initarg :generic-function
    :reader method-generic-function
    :writer (setf m-generic-function))
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
    :accessor method-documentation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-METHOD.

(defclass standard-method (method)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-ACCESSOR-METHOD.

(defclass standard-accessor-method (standard-method)
  ((%slot-definition 
    :initarg :slot-definition
    :reader accessor-method-slot-definition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-READER-METHOD.

(defclass standard-reader-method (standard-accessor-method)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-WRITER-METHOD.

(defclass standard-writer-method (standard-accessor-method)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class METHOD-COMBINATION.

(defclass method-combination (metaobject)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPECIALIZER.

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/specializer-direct-generic-functions.html
(defgeneric specializer-direct-generic-functions (specializer))

(defgeneric specializer-direct-methods (specializer))

(defgeneric eql-specializer-object (eql-specializer))

(defclass specializer (metaobject)
  ((%direct-generic-functions
    :initform '()
    :reader specializer-direct-generic-functions
    :writer (setf direct-generic-functions))
   (%direct-methods
    :initform '()
    :reader specializer-direct-methods
    :writer (setf s-direct-methods))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class EQL-SPECIALIZER.

(defclass eql-specializer (specializer)
  ((%object 
    :initarg :object 
    :reader eql-specializer-object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CLASS.
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

(defclass class (specializer)
  ((%unique-number 
    ;; FIXME: the unique numbers should be assigned during class
    ;; finalization, and not here.
    :initform (prog1 *class-unique-number* (incf *class-unique-number*))
    :reader unique-number)
   (%name 
    :initform nil
    :initarg :name 
    ;; There is a specified function named (SETF CLASS-NAME), but it
    ;; is not an accessor.  Instead it works by calling
    ;; REINITIALIZE-INSTANCE with the new name.
    :reader class-name)
   (%direct-subclasses 
    :initform '() 
    :reader class-direct-subclasses
    :writer (setf c-direct-subclasses))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class REAL-CLASS.

(defclass real-class (class)
  ((%documentation 
    :initform nil
    :accessor c-documentation)
   (%finalized-p 
    :initform nil 
    :reader class-finalized-p
    :writer (setf c-finalized-p))
   (%precedence-list 
    :initform '() 
    ;; The AMOP says that CLASS-PRECEDENCE-LIST should signal an error
    ;; if the class has not been finalized.  We accomplish that effect
    ;; by defining a :BEFORE method that checks this condition, and
    ;; signals an error of the class has not been finalized.
    :reader class-precedence-list
    ;; During class finalization, we need to set the value of this
    ;; slot, so we need a writer for it, and that writer should not be
    ;; named CLASS-PRECEDENCE-LIST because that name is exported.
    ;; Furthermore, also during class finalization, once the class
    ;; precedence list has been computed and store, and we need to
    ;; compute the effective slots and the default initargs, these
    ;; last two steps need to access the precedence list.  However,
    ;; because the function CLASS-PRECEDENCE-LIST signals an error if
    ;; the class is not finalized, those last two steps can not use
    ;; it.  We therefore also need an alternative reader for this slot
    ;; (we could have used SLOT-VALUE, but we prefer a reader which is
    ;; typically faster).  Our solution is to define the ACCESSOR
    ;; named PRECEDENCE-LIST.
    :accessor precedence-list)
   ;; ALLOCATE-INSTANCE and ALLOCATE-BUILT-IN-INSTANCE access this
   ;; slot in order to determine the size of the instance to allocate.
   ;; The writer is used during class finalization. 
   (%instance-size :accessor instance-size)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class REGULAR-CLASS.
;;;
;;; A common superclass of STANDARD-CLASS and
;;; FUNCALLABLE-STANDARD-CLASS.
;;;
;;; This class is not specified by the AMOP, but we are allowed to
;;; define it.  See:
;;; http://metamodular.com/CLOS-MOP/restrictions-on-implementations.html

(defclass regular-class (real-class)
  ((%direct-default-initargs 
    :initarg :direct-default-initargs
    :initform '()
    :reader class-direct-default-initargs
    ;; Additional reader
    :reader direct-default-initargs)
   (%direct-slots 
    :initarg :direct-slots
    :reader direct-slots
    :reader class-direct-slots)
   (%direct-superclasses 
    :initarg :direct-superclasses
    :reader class-direct-superclasses)
   (%default-initargs 
    :reader class-default-initargs
    ;; Additional reader
    :reader default-initargs
    :writer (setf c-default-initargs))
   (%effective-slots 
    :initform '() 
    :reader class-slots
    ;; Additional reader
    :reader effective-slots
    :writer (setf c-slots))
   (%prototype
    :reader class-prototype
    :writer (setf c-prototype))
   (%dependents
    :initform '()
    :accessor dependents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; STANDARD-CLASS.

(defclass standard-class (regular-class)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCALLABLE-STANDARD-CLASS.

(defclass funcallable-standard-class (regular-class)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BUILT-IN-CLASS.
;;;
;;; The AMOP says that the readers CLASS-DIRECT-DEFAULT-INITARGS,
;;; CLASS-DIRECT-SLOTS, CLASS-DEFAULT-INITARGS, and CLASS-SLOTS should
;;; return the empty list for a built-in class.  However, our built-in
;;; classes have direct default initargs, direct slots, default
;;; initargs, and effective slots.  So we keep the slots but we use
;;; different readers: DIRECT-DEFAULT-INITARGS, DIRECT-SLOTS,
;;; DEFAULT-INITARGS, and EFFECTIVE-SLOTS. 

(defclass built-in-class (real-class)
  ((%direct-default-initargs
    :initarg :direct-default-initargs
    :reader direct-default-initargs)
   (%direct-superclasses 
    :initarg :direct-superclasses
    :reader class-direct-superclasses)
   (%direct-slots
    :initarg :direct-slots
    :reader direct-slots)
   (%default-initargs 
    :initarg :default-initargs
    :reader default-initargs
    :writer (setf c-default-initargs))
   (%effective-slots 
    :initform '() 
    :reader effective-slots
    :writer (setf c-slots))))

;;; The AMOP says that CLASS-DIRECT-SLOTS should return the empty list
;;; for a built-in class.
(defmethod class-direct-slots ((class built-in-class))
  (declare (ignore class))
  '())

;;; The AMOP says that CLASS-SLOTS should return the empty list for a
;;; built-in class.
(defmethod class-slots ((class built-in-class))
  (declare (ignore class))
  '())

;;; The AMOP says that CLASS-DEFAULT-INITARGS should return the empty
;;; list for a built-in class.
(defmethod class-default-initargs ((class built-in-class))
  (declare (ignore class))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FORWARD-REFERENCED-CLASS.

(defclass forward-referenced-class (class)
  ((%direct-default-initargs
    ;; The AMOP says that CLASS-DIRECT-DEFAULT-INITARGS should
    ;; return the empty list for a forward referenced class. 
    :allocation :class
    :initform '()
    :reader class-direct-default-initargs)
   (%direct-slots
    ;; The AMOP says that CLASS-DIRECT-SLOTS should return the empty
    ;; list for a forward referenced class.
    :allocation :class
    :initform '()
    :reader direct-slots
    :reader class-direct-slots)
   (%direct-superclasses
    ;; The AMOP says that CLASS-DIRECT-SUPERCLASSES should return the
    ;; empty list for a forward referenced class.
    :allocation :class
    :initform '()
    :reader class-direct-superclasses)
   (%finalized-p
    ;; The AMOP says that CLASS-FINALIZED-P should return false for a
    ;; forward referenced class.
    :allocation :class
    :initform nil
    :reader class-finalized-p)))

(defmethod class-default-initargs ((class forward-referenced-class))
  (declare (ignore class))
  (error "A forward referenced class does not have any default initargs"))

(defmethod class-precedence-list ((class forward-referenced-class))
  (declare (ignore class))
  (error "A forward referenced class does not have a precedence list"))

(defmethod class-slots ((class forward-referenced-class))
  (declare (ignore class))
  (error "A forward referenced class does not have any slots"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Slot definition classes.

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
    :reader slot-definition-initform)
   (%initfunction 
    :initform nil
    :initarg :initfunction
    :reader slot-definition-initfunction)))
