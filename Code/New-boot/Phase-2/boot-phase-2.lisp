(cl:in-package #:sicl-new-boot-phase-2)

;;; We define MAKE-INSTANCE in environment E2 so that it calls the
;;; host MAKE-INSTANCE always with a class metaobject and never with a
;;; symbol.  If our version receives a symbol, it looks up the class
;;; metaobject in environment E1 before calling the host version.
(defun define-make-instance (e1 e2)
  (setf (sicl-genv:fdefinition 'make-instance e2)
        (lambda (class &rest arguments)
          (apply #'make-instance
                 (if (symbolp class)
                     (sicl-genv:find-class class e1)
                     class)
                 arguments))))

(defclass environment (sicl-minimal-extrinsic-environment:environment)
  ())

(defmethod sicl-genv:class-of (object (environment environment))
  (class-of object))

(defun boot-phase-2 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (change-class e2 'environment)
    (sicl-minimal-extrinsic-environment:import-package-from-host 'sicl-clos e3)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'sicl-clos:defclass-expander e2)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     '(setf sicl-genv:special-variable) e2)
    ;; REMOVE is needed by the class initialization protocol.
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'remove e2)
    (load-accessor-defgenerics boot)
    (create-mop-classes boot)
    (define-make-instance e1 e2)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'sicl-genv:find-class e2)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'sicl-genv:typep e2)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'cleavir-code-utilities:proper-list-p e2)))
