(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BUILT-IN-CLASS.
;;;
;;; The AMOP says that the readers CLASS-DIRECT-DEFAULT-INITARGS,
;;; CLASS-DIRECT-SLOTS, CLASS-DEFAULT-INITARGS, and CLASS-SLOTS should
;;; return the empty list for a built-in class.

(defclass built-in-class (real-class)
  ((%direct-superclasses
    :initarg :direct-superclasses
    :reader class-direct-superclasses)
   (%direct-slots
    :initform '()
    :reader class-direct-slots)
   (%finalized-p
    :initform t
    :reader class-finalized-p)
   (%default-initargs
    :initarg :default-initargs
    :accessor class-default-initargs)
   (%effective-slots
    :initform '()
    :reader class-slots)))
