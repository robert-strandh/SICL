(cl:in-package #:sicl-new-boot-phase-2)

(defun define-make-instance (e1 e2)
  (setf (sicl-genv:fdefinition 'make-instance e2)
        (lambda (class &rest arguments)
          (apply #'make-instance
                 (if (symbolp class)
                     (sicl-genv:find-class class e1)
                     class)
                 arguments))))

(defun boot-phase-2 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
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
    (define-make-instance e1 e2)))
