(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class REAL-CLASS.

(defclass real-class (class)
  ((%direct-default-initargs
    :initarg :direct-default-initargs
    :initform '()
    :reader class-direct-default-initargs)
   (%documentation
    :initform nil
    :accessor documentation)
   (%precedence-list
    :initform '()
    ;; The AMOP says that CLASS-PRECEDENCE-LIST should signal an error
    ;; if the class has not been finalized.  We accomplish that effect
    ;; by defining a :BEFORE method that checks this condition, and
    ;; signals an error of the class has not been finalized.
    :reader class-precedence-list
    ;; During class finalization, we need to set the value of this
    ;; slot, so we need a writer for it, and that writer should not be
    ;; named CLASS-PRECEDENCE-LIST because that name is exported.
    ;; Furthermore, also during class finalization, once the class
    ;; precedence list has been computed and store, and we need to
    ;; compute the effective slots and the default initargs, these
    ;; last two steps need to access the precedence list.  However,
    ;; because the function CLASS-PRECEDENCE-LIST signals an error if
    ;; the class is not finalized, those last two steps can not use
    ;; it.  We therefore also need an alternative reader for this slot
    ;; (we could have used SLOT-VALUE, but we prefer a reader which is
    ;; typically faster).  Our solution is to define the ACCESSOR
    ;; named PRECEDENCE-LIST.
    :accessor precedence-list)
   ;; ALLOCATE-INSTANCE and ALLOCATE-BUILT-IN-INSTANCE access this
   ;; slot in order to determine the size of the instance to allocate.
   ;; The writer is used during class finalization.
   (%instance-size :accessor instance-size)))
