(cl:in-package #:sicl-boot-phase-2)

;;; This class will be the metaclass of all the MOP classes defined in
;;; E3.  We define it as a subclass of the host
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
  (format stream "<E3 host class ~a>" (class-name object)))

;;; This method is apparently necessary so that we are allowed to
;;; make instances of our new class.
(defmethod closer-mop:validate-superclass
    ((class funcallable-standard-class)
     (superclass closer-mop:funcallable-standard-class))
  t)

(defun boot (boot)
  (format *trace-output* "Start phase 2~%")
  (with-accessors ((e0 sicl-boot:e0)
                   (e2 sicl-boot:e2))
      boot
    (change-class e2 'environment)
    (change-class (env:client e2) 'client)
    (let ((sicl-client:*client* (make-instance 'client)))
      (import-from-host boot)
      (pre-fill-environment e2)
      (sicl-boot:copy-macro-functions e0 e2)
      (setf (env:fdefinition (env:client e2) e2 'sicl-boot:ast-eval)
            (lambda (client ast)
              (sicl-ast-evaluator:eval-ast client e2 ast)))
      (ensure-asdf-system '#:sicl-cons-compile-time e2)
      (ensure-asdf-system '#:sicl-data-and-control-flow-type-proclamations e2))))
