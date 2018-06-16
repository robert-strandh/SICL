(cl:in-package #:cleavir-load-time-value-hoisting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions on Constructors

(defgeneric creation-form (constructor))

(defgeneric creation-thunk (constructor))

(defgeneric initialization-form (constructor))

(defgeneric initialization-thunk (constructor))

(defgeneric creation-form-finalized-p (constructor))

(defgeneric prototype (constructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Constructor Class and its Subclasses

;;; A constructor is an object that describes how the result of a
;;; load-time-value form or a literal object can be evaluated or
;;; reconstructed at load time.
(defclass constructor ()
  (;; A boolean indicating whether the constructor has been dumped
   (%dumpedp :initarg nil :accessor dumpedp)))

(defclass load-time-value-constructor (constructor)
  ((%form :initarg :form :reader form)))

(defclass object-constructor (constructor)
  ((%prototype :initarg :prototype :reader prototype)))

(defclass user-defined-constructor (object-constructor)
  (;; There must not be circular references within creation forms.  We use
   ;; this boolean to detect such circularities.  Its value is NIL while
   ;; processing the dependencies of the current creation form and T once
   ;; all creation form dependencies have been processed.
   (%creation-form-finalized-p :initform nil :accessor creation-form-finalized-p)
   (%creation-form :initarg :creation-form :reader creation-form)
   (%creation-thunk :accessor creation-thunk)
   (%initialization-form :initarg :initialization-form :reader initialization-form)
   (%initialization-thunk :accessor initialization-thunk)))

(defclass number-constructor (object-constructor)
  ())

(defclass character-constructor (object-constructor)
  ())

(defclass symbol-constructor (object-constructor)
  ())

(defclass package-constructor (object-constructor)
  ())

(defclass random-state-constructor (object-constructor)
  ())

(defclass cons-constructor (object-constructor)
  ())

(defclass array-constructor (object-constructor)
  ())

(defclass hash-table-constructor (object-constructor)
  ())

(defclass pathname-constructor (object-constructor)
  ())
