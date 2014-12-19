(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BUILT-IN-CLASS.
;;;
;;; The AMOP says that the readers CLASS-DIRECT-DEFAULT-INITARGS,
;;; CLASS-DIRECT-SLOTS, CLASS-DEFAULT-INITARGS, and CLASS-SLOTS should
;;; return the empty list for a built-in class.  However, our built-in
;;; classes have direct default initargs, direct slots, default
;;; initargs, and effective slots.  So we keep the slots but we use
;;; different readers: DIRECT-DEFAULT-INITARGS, DIRECT-SLOTS,
;;; DEFAULT-INITARGS, and EFFECTIVE-SLOTS. 

(defclass built-in-class (real-class)
  ((%direct-default-initargs
    :initarg :direct-default-initargs
    :reader direct-default-initargs)
   (%direct-superclasses 
    :initarg :direct-superclasses
    :reader class-direct-superclasses)
   (%direct-slots
    :initarg :direct-slots
    :reader direct-slots)
   (%default-initargs 
    :initarg :default-initargs
    :reader default-initargs
    :writer (setf c-default-initargs))
   (%effective-slots 
    :initform '() 
    :reader effective-slots
    :writer (setf c-slots))))
