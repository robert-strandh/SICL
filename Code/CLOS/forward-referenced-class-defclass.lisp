(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FORWARD-REFERENCED-CLASS.

(defclass forward-referenced-class (class)
  ((%direct-default-initargs
    ;; The AMOP says that CLASS-DIRECT-DEFAULT-INITARGS should
    ;; return the empty list for a forward referenced class. 
    :allocation :class
    :initform '()
    :reader class-direct-default-initargs)
   (%direct-slots
    ;; The AMOP says that CLASS-DIRECT-SLOTS should return the empty
    ;; list for a forward referenced class.
    :allocation :class
    :initform '()
    :reader class-direct-slots)
   (%direct-superclasses
    ;; The AMOP says that CLASS-DIRECT-SUPERCLASSES should return the
    ;; empty list for a forward referenced class.
    :allocation :class
    :initform '()
    :reader class-direct-superclasses)
   (%finalized-p
    ;; The AMOP says that CLASS-FINALIZED-P should return false for a
    ;; forward referenced class.
    :allocation :class
    :initform nil
    :reader class-finalized-p)))
