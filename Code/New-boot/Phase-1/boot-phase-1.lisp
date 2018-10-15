(cl:in-package #:sicl-new-boot-phase-1)

(defclass environment (sicl-minimal-extrinsic-environment:environment)
  ())

(defun import-more-to-env1 (env)
  (import-package-from-host 'sicl-clos env)
  (import-function-from-host 'sicl-clos:defclass-expander env)
  ;; This function is needed because we define a special variable
  ;; *CLASS-UNIQUE-NUMBER* that is used in the :INITFORM of the 
  ;; corresponding slot in the class CLASS.
  (import-function-from-host '(setf sicl-genv:special-variable) env))

(defun boot-phase-1 (boot)
  (format *trace-output* "Start of phase 1~%")
  (with-accessors ((e1 sicl-new-boot:e1) (e2 sicl-new-boot:e2)) boot
    (change-class e1 'environment)
    (import-functions-from-host '(format) e1)
    (setf (sicl-genv:special-variable '*trace-output* e1 t) *trace-output*)
    (import-more-to-env1 e1)
    (import-package-from-host 'sicl-clos e2)
    (load-accessor-defgenerics e2)
    (create-mop-classes-phase1 boot)))
