(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BUILT-IN-CLASS.
;;;
;;; The AMOP says that the readers CLASS-DIRECT-DEFAULT-INITARGS,
;;; CLASS-DIRECT-SLOTS, CLASS-DEFAULT-INITARGS, and CLASS-SLOTS should
;;; return the empty list for a built-in class.  However, our built-in
;;; classes have direct default initargs, direct slots, default
;;; initargs, and effective slots.

(defclass built-in-class (real-class)
  ((%direct-superclasses
    :initarg :direct-superclasses
    :reader class-direct-superclasses)
   (%direct-slots
    :initarg :direct-slots
    :reader class-direct-slots)
   (%default-initargs
    :initarg :default-initargs
    :accessor class-default-initargs)
   (%effective-slots
    :initform '()
    :accessor class-slots)))
