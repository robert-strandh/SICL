(cl:in-package #:sicl-new-boot-phase-1)

(defun import-more-to-env1 (env)
  (sicl-minimal-extrinsic-environment:import-package-from-host 'sicl-clos env)
  (sicl-minimal-extrinsic-environment:import-function-from-host
   'sicl-clos:defclass-expander  env)
  ;; This function is needed because we define a special variable
  ;; *CLASS-UNIQUE-NUMBER* that is used in the :INITFORM of the 
  ;; corresponding slot in the class CLASS.
  (sicl-minimal-extrinsic-environment:import-function-from-host
   '(setf sicl-genv:special-variable) env))

(defun boot-phase-1 ()
  (let ((e1 (make-instance 'sicl-minimal-extrinsic-environment:environment))
        (e2 (make-instance 'sicl-minimal-extrinsic-environment:environment)))
    (import-more-to-env1 e1)
    (sicl-minimal-extrinsic-environment:import-package-from-host 'sicl-clos e2)
    (load-accessor-defgenerics e2)
    (create-mop-classes-phase1 e1 e2)
    (list e1 e2)))
