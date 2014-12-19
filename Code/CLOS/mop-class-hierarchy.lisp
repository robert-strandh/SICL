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
