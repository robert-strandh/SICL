(cl:in-package #:cleavir-load-time-value-hoisting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric creation-form (constructor))

(defgeneric creation-thunk (constructor))

(defgeneric initialization-form (constructor))

(defgeneric initialization-thunk (constructor))

(defgeneric scanning-creation-form-p (constructor))

(defgeneric prototype (constructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

;;; A constructor is an object that describes how the result of a
;;; load-time-value form or a literal object can be evaluated or
;;; reconstructed at load time.
(defclass constructor ()
  (;; A boolean indicating whether the constructor has been dumped
   (%dumpedp :initarg nil :accessor dumpedp)))

(defclass load-time-value-constructor (constructor)
  ((%form :initarg :form :reader form)
   (%creation-thunk :accessor creation-thunk)
   (%read-only-p :initarg :read-only-p :reader read-only-p)))

(defun make-load-time-value-constructor (form read-only-p)
  (make-instance 'load-time-value-constructor
    :form form
    :creation-thunk (compile-hir form)
    :read-only-p read-only-p))

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

(defun make-user-defined-constructor (object)
  (multiple-value-bind (creation-form initialization-form)
      (make-load-form object *compilation-environment*)
    (let ((creation-thunk
            (compile-hir creation-form))
          (initialization-thunk
            (compile-hir initialization-form)))
      (make-instance 'user-defined-constructor
        :creation-form creation-form
        :creation-thunk creation-thunk
        :initialization-form initialization-form
        :initialization-thunk initialization-thunk))))

(defclass standard-constructor (constructor)
  ((%prototype :initarg :prototype :reader prototype)))

(defun make-standard-constructor (object)
  (make-instance 'standard-constructor :prototype object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod scanning-creation-form-p ((constructor constructor))
  nil)

