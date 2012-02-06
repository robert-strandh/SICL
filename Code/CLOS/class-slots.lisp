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
;;; All other other classes are instances of standard-class.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; We give the slot definitions of every metaobject here.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun combine-slots (&rest to-combine)
  (remove-duplicates (reduce #'append to-combine) :from-end t))

;;; Slots for T.
(defparameter *direct-slots-t*
  '())

(defparameter *effective-slots-t*
  *direct-slots-t*)

;;; Slots for STANDARD-OBJECT.
(defparameter *direct-slots-standard-object*
  '((%version-information :initarg :version-information :initform nil)))

(defparameter *effective-slots-standard-object*
  (combine-slots *effective-slots-t*
		 *direct-slots-standard-object*))

;;; Slots for FUNCTION.
(defparameter *direct-slots-function*
  '())

(defparameter *effective-slots-function*
  *direct-slots-function*)

;;; Slots for FUNCALLABLE-STANDARD-OBJECT.
(defparameter *direct-slots-funcallable-standard-object*
  ;; FIXME, maybe add something here? 
  '())

(defparameter *effective-slots-funcallable-standard-object*
  (combine-slots *effective-slots-standard-object*
		 *effective-slots-function*
		 *direct-slots-funcallable-standard-object*))

;;; Slots for METAOBJECT.
(defparameter *direct-slots-metaobject*
  '())

(defparameter *effective-slots-metaobject*
  (combine-slots *effective-slots-standard-object*
		 *direct-slots-metaobject*))

;;; Slots for GENERIC-FUNCTION.
(defparameter *direct-slots-generic-function*
  ;; FIXME: add more here
  '())

(defparameter *effective-slots-generic-function*
  (combine-slots *effective-slots-metaobject*
		 *effective-slots-funcallable-standard-object*
		 *direct-slots-generic-function*))

;;; Slots for STANDARD-GENERIC-FUNCTION.
(defparameter *direct-slots-standard-generic-function*
  '())

(defparameter *effective-slots-standard-generic-function*
  (combine-slots *effective-slots-generic-function*
		 *direct-slots-standard-generic-function*))

;;; Slots for METHOD
(defparameter *direct-slots-method*
  '())

(defparameter *effective-slots-method*
  (combine-slots *effective-slots-metaobject*
		 *direct-slots-method*))

;;; Slots for STANDARD-METHOD
(defparameter *direct-slots-standard-method*
  '())

(defparameter *effective-slots-standard-method*
  (combine-slots *effective-slots-method*
		 *direct-slots-standard-method*))

;;; Slots for STANDARD-ACCESSOR-METHOD
(defparameter *direct-slots-standard-accessor-method*
  '())

(defparameter *effective-slots-standard-accessor-method*
  (combine-slots *effective-slots-standard-method*
		 *direct-slots-standard-accessor-method*))

;;; Slots for STANDARD-READER-METHOD
(defparameter *direct-slots-standard-reader-method*
  '())

(defparameter *effective-slots-standard-reader-method*
  (combine-slots *effective-slots-standard-accessor-method*
		 *direct-slots-standard-reader-method*))

;;; Slots for STANDARD-WRITER-METHOD
(defparameter *direct-slots-standard-writer-method*
  '())

(defparameter *effective-slots-standard-writer-method*
  (combine-slots *effective-slots-standard-accessor-method*
		 *direct-slots-standard-writer-method*))

;;; Slots for METHOD-COMBINATION
(defparameter *direct-slots-method-combination*
  '())

(defparameter *effective-slots-method-combination*
  (combine-slots *effective-slots-metaobject*
		 *direct-slots-method-combination*))

;;; Slots for SLOT-DEFINITION
(defparameter *direct-slots-slot-definition*
  '((%name :initarg :name :reader slot-definition-name)
    (%allocation :initarg :allocation
		 :initform :instance
		 :reader slot-definition-allocation)
    (%type :initarg :type
	   :initform t
	   :reader slot-definition-type)
    (%initargs :initarg :initargs :reader slot-definition-initargs)
    (%initform :initarg :initform :reader slot-definition-initform)
    (%initfunction :accessor slot-definition-initfunction)
    (%readers :initarg :readers :reader slot-definition-readers)
    (%writers :initarg :writers :reader slot-definition-writers)))

(defparameter *effective-slots-slot-definition*
  (combine-slots *effective-slots-metaobject*
		 *direct-slots-slot-definition*))

;;; Slots for DIRECT-SLOT-DEFINITION
(defparameter *direct-slots-direct-slot-definition*
  '())

(defparameter *effective-slots-direct-slot-definition*
  (combine-slots *effective-slots-slot-definition*
		 *direct-slots-direct-slot-definition*))

;;; Slots for EFFECTIVE-SLOT-DEFINITION
(defparameter *direct-slots-effective-slot-definition*
  '((%position :initarg :position :reader slot-definition-position)))

(defparameter *effective-slots-effective-slot-definition*
  (combine-slots *effective-slots-slot-definition*
		 *direct-slots-effective-slot-definition*))

;;; Slots for STANDARD-SLOT-DEFINITION
(defparameter *direct-slots-standard-slot-definition*
  '())

(defparameter *effective-slots-standard-slot-definition*
  (combine-slots *effective-slots-slot-definition*
		 *direct-slots-standard-slot-definition*))

;;; Slots for STANDARD-DIRECT-SLOT-DEFINITION
(defparameter *direct-slots-standard-direct-slot-definition*
  '())

(defparameter *effective-slots-standard-direct-slot-definition*
  (combine-slots *effective-slots-standard-slot-definition*
		 *effective-slots-direct-slot-definition*
		 *direct-slots-standard-direct-slot-definition*))

;;; Slots for STANDARD-EFFECTIVE-SLOT-DEFINITION
(defparameter *direct-slots-standard-effective-slot-definition*
  '())

(defparameter *effective-slots-standard-effective-slot-definition*
  (combine-slots *effective-slots-standard-slot-definition*
		 *effective-slots-effective-slot-definition*
		 *direct-slots-standard-effective-slot-definition*))

;;; Slots for SPECIALIZER
(defparameter *direct-slots-specializer*
  '())

(defparameter *effective-slots-specializer*
  (combine-slots *effective-slots-metaobject*
		 *direct-slots-specializer*))

;;; Slots for EQL-SPECIALIZER
(defparameter *direct-slots-eql-specializer*
  '())

(defparameter *effective-slots-eql-specializer*
  (combine-slots *effective-slots-specializer*
		 *direct-slots-eql-specializer*))

;;; Slots for CLASS
(defparameter *direct-slots-class*
  '((%unique-number :initform nil :accessor class-unique-number)))

(defparameter *effective-slots-class*
  (combine-slots *effective-slots-specializer*
		 *direct-slots-class*))

;;; Slots for BUILT-IN-CLASS
(defparameter *direct-slots-built-in-class*
  '())

(defparameter *effective-slots-built-in-class*
  (combine-slots *effective-slots-class*
		 *direct-slots-built-in-class*))

;;; Slots for FORWARD-REFERENCE-CLASS
(defparameter *direct-slots-forward-reference-class*
  '())

(defparameter *effective-slots-forward-reference-class*
  (combine-slots *effective-slots-class*
		 *direct-slots-forward-reference-class*))

;;; Slots for STANDARD-CLASS
(defparameter *direct-slots-standard-class*
  '((%name :initarg :name :accessor class-name)
    (%direct-superclasses :accessor class-direct-superclasses)
    (%direct-slots :accessor class-direct-slots)
    (%precedence-list :initform '() :accessor class-precedence-list)
    ;; For some reason, this accessor is not called
    ;; class-effective-slots.
    (%effective-slots :initform '() :accessor class-slots)
    (%direct-subclasses :initform '() :accessor class-direct-subclasses)
    (%direct-methods :initform '() :accessor class-direct-methods)))

(defparameter *effective-slots-standard-class*
  (combine-slots *effective-slots-class*
		 *direct-slots-standard-class*))

;; Slots for FUNCALLABLE-STANDARD-CLASS
(defparameter *direct-slots-funcallable-standard-class*
  '())

(defparameter *effective-slots-funcallable-standard-class*
  (combine-slots *effective-slots-class*
		 *direct-slots-funcallable-standard-class*))

) ; eval-when