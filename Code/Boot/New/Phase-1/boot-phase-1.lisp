(cl:in-package #:sicl-boot-phase-1)

;;; This class will be the metaclass of all the MOP classes defined in
;;; E2.  We define it as a subclass of the host
;;; FUNCALLABLE-STANDARD-CLASS so that instances of it can be used as
;;; functions, and so that we avoid any problems with incompatible
;;; superclasses and metaclasses.
;;;
;;; Furthermore, this class will be the specializer of the :AROUND
;;; method on SHARED-INITIALIZE that handles the class-initialization
;;; protocol.  Recall that this :AROUND method handles the definitions
;;; of slot readers and slot writers defined in class slots.
;;; Therefore, that aspect of the host class-initialization protocol
;;; will not be invoked.
(defclass funcallable-standard-class
    (closer-mop:funcallable-standard-class)
  ())

(defmethod print-object ((object funcallable-standard-class) stream)
  (format stream "<E2 host class ~a>" (class-name object)))

;;; This method is apparently necessary so that we are allowed to
;;; make instances of our new class.
(defmethod closer-mop:validate-superclass
    ((class funcallable-standard-class)
     (superclass closer-mop:funcallable-standard-class))
  t)

(defun boot (boot)
  (declare (ignore boot)))
