(cl:in-package #:cleavir-load-time-value-hoisting)

;;; A constructor is an object that describes how a load-time-value form or
;;; a literal object can be evaluated or reconstructed at load time.

(defclass constructor ()
  ())

(defclass load-time-value-constructor (constructor)
  ((%form :initarg :form :reader form)
   (%creation-thunk :initarg :creation-thunk :accessor creation-thunk)
   (%read-only-p :initarg :read-only-p :reader read-only-p)))

(defclass user-defined-constructor (constructor)
  (;; There must not be circular references within creation forms.  We use
   ;; this boolean to detect such circularities.  Its value is T while
   ;; processing the dependencies of the current creation form and NIl once
   ;; all creation form dependencies have been processed.
   (%scanning-creation-form-p :initform t :accessor scanning-creation-form-p)
   (%creation-form :initarg :creation-form :reader creation-form)
   (%creation-thunk :initarg :creation-thunk :accessor creation-thunk)
   (%initialization-form :initarg :initialization-form :reader initialization-form)
   (%initialization-thunk :initarg :initialization-thunk :accessor initialization-thunk)))

(defclass standard-constructor (constructor)
  ((%prototype :initarg :prototype :reader prototype)))

(defun make-standard-constructor (object)
  (make-instance 'standard-constructor :prototype object))

(defmethod scanning-creation-form-p ((constructor constructor))
  nil)
