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
;;; forward-reference-class            (class)
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
;;; Accessors for class metaobjects.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLASS-NAME and (SETF CLASS-NAME)

;;; The name of a class can be any object, but it is usually a symbol.
;;; If it is NIL, then it means that the class does not have a name.
;;; For a subclass of STANDARD-CLASS, FUNCALLABLE-STANDARD-CLASS, or
;;; FORWARD-REFERENCE-CLASS, this function returns the defaulted value
;;; of the argument :NAME, given when the class was initialized or
;;; reinitialized.  For a subclass of BUILT-IN-CLASS, it returns the
;;; name of the built-in class. 
(defgeneric class-name (class))

;;; According to the AMOP, this function should call
;;; REINITIALIZE-INSTANCE with three arguments: 
;;; CLASS, :NAME, and NEW-NAME.
(defgeneric (setf class-name) (new-name class))

;;; Return the defaulted value of the argument :DIRECT-SUPERCLASSES
;;; given when the class was initialized or reinitialized.  The value
;;; is a list of class metaobjects, i.e. subclasses of the class
;;; CLASS.
(defgeneric class-direct-superclasses (class))

;;; Return the defaulted value of the argument :DIRECT-SLOTS given
;;; when the class was initialized or reinitialized.  The value is a
;;; list of direct slot definition metaobjects, i.e., subclasses of
;;; the class DIRECT-SLOT-DEFINITION. 
(defgeneric class-direct-slots (class))

;;; Return a SET (represented as alist, but with the elements in no
;;; particular order) of subclasses of the class.
(defgeneric class-direct-subclasses (class))

;;; The functions ADD-DIRECT-SUBCLASS and REMOVE-DIRECT-SUBCLASS are
;;; used to update the direct subclasses of a class, so this function
;;; is not part of the public interface and should be used only from
;;; these two functions. 
(defgeneric (setf class-direct-subclasses) (new-value class))

;;; FIXME: write more comments
(defgeneric class-default-initargs (class))

;;; FIXME: write more comments
(defgeneric class-slots (class))

;;; FIXME: write more comments
(defgeneric class-precedence-list (class))

;;; FIXME: write more comments
(defgeneric class-finalized-p (class))

;;; FIXME: write more comments
(defgeneric class-prototype (class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors for generic-function metaobjects.

(defgeneric generic-function-argument-precedence-order (generic-function))

(defgeneric generic-function-declarations (generic-function))

(defgeneric generic-function-lambda-list (generic-function))

(defgeneric generic-function-method-class (generic-function)) 

(defgeneric generic-function-method-combination (generic-function))

(defgeneric generic-function-methods (generic-function)) 

(defgeneric generic-function-name (generic-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors for method metaobjects.

(defgeneric method-function (generic-function))

(defgeneric method-generic-function (generic-function))

(defgeneric method-lambda-list (generic-function))

(defgeneric method-specializers (generic-function))

(defgeneric method-qualifiers (generic-function))

(defgeneric accessor-method-slot-definition (generic-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors for slot definition metaobjects.

(defgeneric slot-definition-allocation (generic-function))

(defgeneric slot-definition-initargs (generic-function))

(defgeneric slot-definition-initform (generic-function))

(defgeneric slot-definition-initfunction (generic-function))

(defgeneric slot-definition-name (generic-function))

(defgeneric slot-definition-type (generic-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The MOP class hierarchy. 

(defconstant +timestamp-offset+ 0)
(defconstant +instance-slots-offset+ 1)

(defclass standard-object (t)
  (;; Every standard instance has a unique time stamp.  The next
   ;; available consecutive timestamp is given to an instance when it
   ;; is initialized or reinitialized.  Instances of a class hence get
   ;; a timestamp that is greater than that of the class, unless the
   ;; class is reinitialized after instances have been created.  To
   ;; check whether an instance needs to be updated, we simply check
   ;; the timestamp against that of its class.  This is done whenever
   ;; CLASS-OF is used on an instance, so that essentially nothing can
   ;; be done to an instance without this check being made.  It is
   ;; important that the timestamp be the first slot of the instance
   ;; so that low-level code knows where to look for it.
   (%timestamp
    :accessor standard-instance-timestamp)
   ;; We keep a copy of the effective slots of the class that were
   ;; used to create an instance.  They are used when the instance
   ;; needs to be updated because the class has changed, and it may
   ;; also be used by the garbage collector for type information of
   ;; the existing slots, etc.
   (%instance-slots
    :initarg :instance-slots
    :initform nil
    :accessor standard-instance-slots)))

(define-built-in-class t ())

(define-built-in-class function (t))

(defclass funcallable-standard-object (standard-object function)
  ())

(defclass metaobject (standard-object)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GENERIC-FUNCTION and STANDARD-GENERIC-FUNCTION.

(defclass generic-function (metaobject funcallable-standard-object)
  ((%name 
    :reader generic-function-name
    :writer (setf gf-name))
   (%lambda-list 
    :reader generic-function-lambda-list
    :writer (setf gf-lambda-list))
   (%documentation 
    :accessor gf-documentation)
   (%discriminating-function 
    :accessor discriminating-function)
   (%dependents
    :initform '()
    :accessor dependents))
  (:metaclass funcallable-standard-class))

(defclass standard-generic-function (generic-function)
  ((%argument-precedence-order
    :reader generic-function-argument-precedence-order
    :writer (setf gf-argument-precedence-order))
   (%declarations 
    :reader generic-function-declarations
    :writer (setf gf-declarations))
   (%method-class 
    :reader generic-function-method-class
    :writer (setf gf-method-class))
   (%method-combination 
    :reader generic-function-method-combination
    :writer (setf gf-method-combination))
   (%methods 
    :initform '() 
    :reader generic-function-methods
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
    :accessor specializer-profile)
   (%set-cache
    :accessor set-cache))
  (:metaclass funcallable-standard-class))

(defclass method (metaobject)
  ())

(defclass standard-method (method)
  ((%function 
    :initarg :function 
    :reader method-function)
   (%generic-function 
    :initarg :generic-function
    :accessor method-generic-function)
   (%lambda-list 
    :initarg :lambda-list 
    :accessor method-lambda-list)
   (%specializers 
    :initarg :specializers 
    :accessor method-specializers)
   (%qualifiers 
    :initarg :qualifiers 
    :accessor method-qualifiers)))

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
    :accessor specializer-direct-generic-functions)
   (%direct-methods
    :initform '()
    :accessor specializer-direct-methods)))

(defclass eql-specializer (specializer)
  ((%object 
    :initarg :object 
    :reader eql-specializer-object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLASS.

(defparameter *class-unique-number* 0)

(defclass class (specializer)
  ((%unique-number 
    :initform (prog1 *class-unique-number* (incf *class-unique-number*))
    :reader unique-number)
   (%name 
    :initform nil
    :initarg :name 
    ;; Make this just a :reader, because the AMOP says that the
    ;; function (SETF CLASS-NAME) does not just set the name of the
    ;; class.
    :reader class-name
    :writer (setf c-name))
   (%direct-subclasses 
    :initform '() 
    :reader class-direct-subclasses
    :writer (setf c-direct-subclasses))
   (%direct-methods 
    :initform '() 
    :reader class-direct-methods
    :writer (setf c-direct-methods))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUILT-IN-CLASS.

(defclass built-in-class (class)
  ((%direct-default-initargs
    ;; The AMOP says that CLASS-DIRECT-DEFAULT-INITARGS should
    ;; return the empty list for a built-in class.
    :allocation :class
    :initform '()
    :reader class-direct-default-initargs)
   (%direct-superclasses 
    ;; I am adding this :initarg as a patch that I don't know if
    ;; it is the right thing to do -- RS 2013-03-12
    :initarg :direct-superclasses
    :reader class-direct-superclasses
    :writer (setf c-direct-superclasses))
   (%direct-slots
    ;; The AMOP says that CLASS-DIRECT-SLOTS should return the empty
    ;; list for a built-in class.
    :allocation :class
    :initform '()
    :reader class-direct-slots)
   (%documentation 
    :accessor c-documentation)
   (%precedence-list 
    :initarg :precedence-list 
    :reader class-precedence-list)
    ;; The AMOP says that CLASS-DEFAULT-INITARGS should return the
    ;; empty list for a built-in class.
   (%default-initargs 
    :allocation :class
    :initform '()
    :reader class-default-initargs)
    ;; The AMOP says that CLASS-SLOTS should return the empty list for
    ;; a built-in class.
   (%effective-slots 
    :allocation :class
    :initform '() 
    :reader class-slots)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FORWARD-REFERENCE-CLASS.

(defclass forward-reference-class (class)
  ((%direct-default-initargs
    ;; The AMOP says that CLASS-DIRECT-DEFAULT-INITARGS should
    ;; return the empty list for a forward reference class. 
    :allocation :class
    :initform '()
    :reader class-direct-default-initargs)
   (%direct-slots
    ;; The AMOP says that CLASS-DIRECT-SLOTS should return the empty
    ;; list for a forward reference class.
    :allocation :class
    :initform '()
    :reader class-direct-slots)
   (%direct-superclasses
    ;; The AMOP says that CLASS-DIRECT-SUPERCLASSES should return the
    ;; empty list for a forward reference class.
    :allocation :class
    :initform '()
    :reader class-direct-superclasses)
   (%finalized-p
    ;; The AMOP says that CLASS-FINALIZED-P should return false for a
    ;; forward reference class.
    :allocation :class
    :initform nil
    :reader class-finalized-p)))

(defmethod class-default-initargs ((class forward-reference-class))
  (error "A forward reference class does not have any default initargs"))

(defmethod class-precedence-list ((class forward-reference-class))
  (error "A forward reference class does not have a precedence list"))

(defmethod class-slots ((class forward-reference-class))
  (error "A forward reference class does not have any slots"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; STANDARD-CLASS.

;;; FIXME: it looks like some code factoring would be useful here.
(defclass standard-class (class)
  ((%direct-default-initargs 
    :reader class-direct-default-initargs
    :writer (setf c-direct-default-initargs))
   (%direct-slots 
    :reader class-direct-slots
    :writer (setf c-direct-slots))
   (%direct-superclasses 
    ;; This slot has no initform and no initarg, because it is set by
    ;; the :after method on INITIALIZE-INSTANCE using the
    ;; :direct-superclasses initarg.
    :reader class-direct-superclasses
    :writer (setf c-direct-superclasses))
   (%documentation 
    :accessor c-documentation)
   (%finalized-p 
    :initform nil 
    :accessor class-finalized-p)
   (%precedence-list 
    :initform '() 
    :accessor class-precedence-list)
   (%default-initargs 
    :reader class-default-initargs
    :writer (setf c-default-initargs))
   ;; For some reason, this accessor is not called
   ;; class-effective-slots.
   (%effective-slots 
    :initform '() 
    :reader class-slots
    :writer (setf c-slots))
   (%prototype
    :reader class-prototype
    :writer (setf c-prototype))
   (%dependents
    :initform '()
    :accessor dependents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCALLABLE-STANDARD-CLASS.

(defclass funcallable-standard-class (class)
  ((%direct-default-initargs 
    :initform '()
    :initarg :direct-default-initargs
    :reader class-direct-default-initargs)
   (%direct-slots 
    :initarg :direct-slots
    :accessor class-direct-slots)
   (%direct-superclasses 
    ;; This slot has no initform and no initarg, because it is set by
    ;; the :after method on INITIALIZE-INSTANCE using the
    ;; :direct-superclasses initarg.
    :reader class-direct-superclasses
    :writer (setf c-direct-superclasses))
   (%documentation 
    :accessor c-documentation)
   (%finalized-p 
    :initform nil 
    :reader class-finalized-p
    :writer (setf c-default-initargs))
   (%precedence-list 
    :initform '() 
    :accessor class-precedence-list)
   (%default-initargs 
    :reader class-default-initargs
    :writer (setf c-default-initargs))
   ;; For some reason, this accessor is not called
   ;; class-effective-slots.
   (%effective-slots 
    :initform '() 
    :accessor class-slots)
   (%dependents
    :initform '()
    :accessor dependents)))
