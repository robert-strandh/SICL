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

;;; Map from names of classes to direct slot descriptors
(defparameter *direct-slots* (make-hash-table :test #'eq))

(defun find-direct-slots (class-name)
  (gethash class-name *direct-slots*))

(defun (setf find-direct-slots) (direct-slots class-name)
  (setf (gethash class-name *direct-slots*) direct-slots))

;;; Map from names of classes to effective slot descriptors
(defparameter *effective-slots* (make-hash-table :test #'eq))

(defun find-effective-slots (class-name)
  (gethash class-name *effective-slots*))

(defun (setf find-effective-slots) (effective-slots class-name)
  (setf (gethash class-name *effective-slots*) effective-slots))

(defun define-slots (class-name superclass-names direct-slots)
  (setf (find-direct-slots class-name) direct-slots)
  (setf (find-effective-slots class-name)
	(remove-duplicates
	 (append (loop for superclass-name in (reverse superclass-names)
		       append (find-effective-slots superclass-name))
		 direct-slots)
	 :from-end t)))

(define-slots 't '() '())

(define-slots 'standard-object '(t)
  '((%version-information :initarg :version-information :initform nil)))

(define-slots 'function '()
  '())

(define-slots 'funcallable-standard-object '()
  ;; FIXME, maybe add something here? 
  '())

(define-slots 'metaobject '(standard-object)
  '())

(define-slots 'generic-function '(metaobject funcallable-standard-object)
  ;; FIXME: add more here
  '())

(define-slots 'standard-generic-function '(generic-function)
  '())

(define-slots 'method '(metaobject)
  '())

(define-slots 'standard-method '(method)
  '())

(define-slots 'standard-accessor-method '(standard-method)
  '())

(define-slots 'standard-reader-method '(standard-accessor-method)
  '())

(define-slots 'standard-writer-method '(standard-accessor-method)
  '())

(define-slots 'method-combination '(metaobject)
  '())

(define-slots 'slot-definition '(metaobject)
  '((%name :initarg :name :reader slot-definition-name)
    (%allocation :initarg :allocation
		 :initform :instance
		 :reader slot-definition-allocation)
    (%type :initarg :type
	   :initform t
	   :reader slot-definition-type)
    (%initargs :initarg :initargs :reader slot-definition-initargs)
    (%initform :initarg :initform :reader slot-definition-initform)
    (%initfunction :accessor slot-definition-initfunction)))

;;; The READERS and WRITERS slots only exist in direct slot
;;; definitions, because they are not combined the way other slot
;;; properties are when an effective slot definition is computer.
(define-slots 'direct-slot-definition '(slot-definition)
  '((%readers :initarg :readers :reader slot-definition-readers)
    (%writers :initarg :writers :reader slot-definition-writers)))

(define-slots 'effective-slot-definition '(slot-definition)
  '((%position :initarg :position :reader slot-definition-position)))

(define-slots 'standard-slot-definition '(slot-definition)
  '())

(define-slots 'standard-direct-slot-definition
  '(slot-definition direct-slot-definition)
  '())

(define-slots 'standard-effective-slot-definition
  '(slot-definition effective-slot-definition)
  '())

(define-slots 'specializer '(metaobject)
  '())

(define-slots 'eql-specializer '(specializer)
  '())

(define-slots 'class '(specializer)
  '((%unique-number :initform nil :accessor class-unique-number)))

(define-slots 'built-in-class '(class)
  '())

(define-slots 'forward-reference-class '(class)
  '())

(define-slots 'standard-class '(class)
'((%name :initarg :name :accessor class-name)
    (%direct-superclasses :accessor class-direct-superclasses)
    (%direct-slots :accessor class-direct-slots)
    (%precedence-list :initform '() :accessor class-precedence-list)
    ;; For some reason, this accessor is not called
    ;; class-effective-slots.
    (%effective-slots :initform '() :accessor class-slots)
    (%direct-subclasses :initform '() :accessor class-direct-subclasses)
    (%direct-methods :initform '() :accessor class-direct-methods)))

(define-slots 'funcallable-standard-class '(class)
  '())

) ; eval-when
