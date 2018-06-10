(cl:in-package #:cleavir-load-time-value-hoisting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Constructor Class and its Subclasses
;;;
;;; A constructor is an object that describes how the result of a
;;; load-time-value form or a literal object can be evaluated or
;;; reconstructed at load time.

(defclass constructor ()
  ())

;;; A user defined constructor is the constructor of a structure or a
;;; standard object for which there exists an applicable method on
;;; MAKE-LOAD-FORM.
(defclass user-defined-constructor (constructor)
  (;; There must not be circular references within creation forms.  We use
   ;; this boolean to detect such circularities.  Its value is NIL while
   ;; processing the dependencies of the current creation form and T once
   ;; all creation form dependencies have been processed.
   (%creation-form-finalized-p :initform nil :accessor creation-form-finalized-p)
   ;; The first return value of MAKE-LOAD-FORM
   (%creation-form :initarg :creation-form :reader creation-form)
   ;; The second return value of MAKE-LOAD-FORM
   (%initialization-form :initarg :initialization-form :reader initialization-form)))

;;; A constructor for an object that qualifies for coalescing.
(defclass coalescible-constructor (constructor)
  (;; The object to be reconstructed.
   (%object :initarg :object :reader object)
   ;; A list of constructors. Two constructors of the same class and with
   ;; EQUAL dependencies construct similar objects.
   (%dependencies :initarg :dependecies :accessor dependencies)
   ;; The list of constructors that have this constructor in their
   ;; dependencies.
   (%users :initform '() :accessor users)))

(defclass load-time-value-constructor (constructor)
  ((%form :initarg :form :reader form)))

(defclass number-constructor (constructor)
  ())

(defclass character-contructor (constructor)
  ())

(defclass symbol-constructor (coalescible-constructor)
  ())

(defclass package-constructor (coalescible-constructor)
  ())

(defclass random-state-constructor (constructor)
  ())

(defclass cons-constructor (coalescible-constructor)
  ())

(defclass array-constructor (coalescible-constructor)
  ())

(defclass hash-table-constructor (constructor)
  ())

(defclass pathname-constructor (constructor)
  ())
