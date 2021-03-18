(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class REAL-CLASS.

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-precedence-list.html
(defgeneric class-precedence-list (class))

;;; This function is similar to CLASS-PRECEDENCE-LIST, except that
;;; CLASS-PRECEDENCE-LIST is specified to signal an error when the
;;; class is not finalized, which we accomplish by using a :BEFORE
;;; method.  However, during class finalization, we need to access the
;;; class precedence list in the two steps following its computation.
;;; Since at that point the class is not yet finalized, those two
;;; steps can not call CLASS-PRECEDENCE-LIST.  Our solution is to
;;; define an alternative reader for the same slot, named
;;; PRECEDENCE-LIST, and which does not signal an error if the class
;;; is not finalized.
(defgeneric precedence-list (class))

;;; This function is used by the class finalization protocol to set
;;; the precedence list of the class.
(defgeneric (setf precedence-list) (precedence-list class))

(defclass real-class (class)
  ((%direct-default-initargs
    :initarg :direct-default-initargs
    :initform '()
    :reader class-direct-default-initargs)
   (%documentation
    :initform nil
    :accessor documentation)
   (%direct-superclasses
    :initarg :direct-superclasses
    :reader class-direct-superclasses)
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
