(cl:in-package #:sicl-boot-phase-2)

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
  (format *trace-output* "Start phase 2~%")
  (with-accessors ((e0 sicl-boot:e0)
                   (e1 sicl-boot:e1)
                   (e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)) 
      boot
    (change-class e2 'environment)
    (import-from-host boot)
    (sicl-boot:copy-macro-functions e0 e3)
    (setf (sicl-boot:overridden-function-cells e3)
          `((find-class
             . (,(lambda (name &optional (errorp t) environment)
                   (declare (ignore environment))
                   (if (eq name 't)
                       (find-class 't)
                       (let ((result (env:find-class (env:client e2) e2 name)))
                         (if (and (null result) errorp)
                             (error "no class named ~s in E2" name)
                             result))))))
            (add-method
             . (,#'add-method))
            (make-instance
             . (,#'make-instance))))
    (import-functions-from-host '(closer-mop:method-function) e3)
    (let ((client (env:client e3)))
      (setf (env:fdefinition client e3 'sicl-boot:ast-eval)
            (lambda (ast)
              (let ((code (sicl-ast-evaluator:translate-top-level-ast ast)))
                (funcall (compile nil code) e3)))))
    (prepare-next-phase boot)))
