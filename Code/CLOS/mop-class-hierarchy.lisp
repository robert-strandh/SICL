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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The MOP class hierarchy. 

(defconstant +class-unique-number-offset+ 0)
(defconstant +instance-slots-offset+ 1)

(define-built-in-class t () ())

(define-built-in-class function (t)
  ((%entry-point :initarg :entry-point)
   (%linkage-vector :initarg :linkage-vector)
   (%environment :initform nil :initarg :environment)))

(defclass standard-object (t) ())

(defclass funcallable-standard-object (standard-object function)
  ())

(defclass metaobject (standard-object)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GENERIC-FUNCTION and STANDARD-GENERIC-FUNCTION.
;;;
;;; FIXME: I can not remember why I decided not to use initargs for
;;; the slots here, and instead calling explicit writers in :AFTER
;;; methods on INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE.

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
   (%discriminating-function 
    :accessor discriminating-function)
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
    :reader generic-function-method-class
    :writer (setf gf-method-class))
   (%method-combination 
    :initarg :method-combination
    ;; FIXME: remove this later.
    :initform nil
    :reader generic-function-method-combination
    :writer (setf gf-method-combination))
   (%methods 
    :initform '() 
    ;; This reader is the one that the AMOP specifies.
    :reader generic-function-methods
    ;; Additional reader.
    :reader gf-methods
    :writer (setf gf-methods))
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
    :accessor specializer-profile)
   (%set-cache
    :accessor set-cache))
  (:metaclass funcallable-standard-class))

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

(defclass standard-method (method)
  ())

(defclass standard-accessor-method (standard-method)
  ((%slot-definition 
    :initarg :slot-definition
    :reader accessor-method-slot-definition)))

(defclass standard-reader-method (standard-accessor-method)
  ())

(defclass standard-writer-method (standard-accessor-method)
  ())

(defclass method-combination (metaobject)
  ())

(defclass specializer (metaobject)
  ((%direct-generic-functions
    :initform '()
    :reader specializer-direct-generic-functions
    :writer (setf s-direct-generic-functions))
   (%direct-methods
    :initform '()
    :reader specializer-direct-methods
    :writer (setf s-direct-methods))))

(defclass eql-specializer (specializer)
  ((%object 
    :initarg :object 
    :reader eql-specializer-object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CLASS.

(defparameter *class-unique-number* 0)

(defclass class (specializer)
  ((%unique-number 
    :initform (prog1 *class-unique-number* (incf *class-unique-number*))
    :reader unique-number)
   (%name 
    :initform nil
    :initarg :name 
    ;; There is a specified function named (SETF CLASS-NAME), but it
    ;; is not an accessor.  Instead it works by calling
    ;; REINITIALIZE-INSTANCE with the new name.
    :reader class-name
    :writer (setf c-name))
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
    :reader class-precedence-list
    :writer (setf c-precedence-list))))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUILT-IN-CLASS.
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
;;; FORWARD-REFERENCED-CLASS.

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

;;; The READERS and WRITERS slots only exist in direct slot
;;; definitions, because they are not combined the way other slot
;;; properties are when an effective slot definition is computer.
(defclass direct-slot-definition (slot-definition)
  ((%readers 
    :initform '()
    :initarg :readers 
    :reader slot-definition-readers)
   (%writers 
    :initform '()
    :initarg :writers 
    :reader slot-definition-writers)
   ;; The CAR of the CONS cell stored in this slot is used to hold
   ;; values when :ALLOCATION is :CLASS.  In this case, the CONS cell
   ;; becomes the location of the effective slot definition.
   (%storage
    :initform (list nil)
    :reader slot-definition-storage)))

(defclass effective-slot-definition (slot-definition)
  ((%location 
    :initform nil
    :initarg :location 
    :reader slot-definition-location
    :writer (setf s-location))))

(defclass standard-slot-definition (slot-definition)
  ())

(defclass standard-direct-slot-definition
    (standard-slot-definition direct-slot-definition)
  ())

(defclass standard-effective-slot-definition
    (standard-slot-definition effective-slot-definition)
  ())
