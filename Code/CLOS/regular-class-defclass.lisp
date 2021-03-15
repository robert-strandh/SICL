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
  ((%direct-slots
    :initarg :direct-slots
    :reader class-direct-slots)
   (%finalized-p
    :initform nil
    :accessor class-finalized-p)
   (%default-initargs
    :accessor class-default-initargs)
   (%effective-slots
    :initform '()
    :accessor class-slots)
   (%prototype
    :accessor class-prototype)
   (%dependents
    :initform '()
    :accessor dependents)))
