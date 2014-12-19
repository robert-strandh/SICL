(cl:in-package #:sicl-clos)
   
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
