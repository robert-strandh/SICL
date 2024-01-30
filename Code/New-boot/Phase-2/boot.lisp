(cl:in-package #:sicl-new-boot-phase-2)

(defun boot (boot)
  (let* ((client (make-instance 'client))
         (environment (create-environment client))
         (global-environment
           (trucler:global-environment client environment)))
    (sb:define-package-functions client global-environment)
    (setf (sb:e2 boot) global-environment)
    (reinitialize-instance client
      :environment global-environment)
    (sb:define-backquote-macros client global-environment)
    (import-from-host client global-environment)
    (sb:import-khazern client global-environment)
    (sb:define-environment-functions client global-environment)
    (clo:make-variable
     client global-environment '*package* (find-package '#:common-lisp-user))    (sb:ensure-asdf-system
     client environment "sicl-environment-package")
    (let ((environment-symbol
            (sb:intern-parcl-symbol
             client "SICL-ENVIRONMENT" "*ENVIRONMENT*"))
          (client-symbol
            (sb:intern-parcl-symbol
             client "SICL-ENVIRONMENT" "*CLIENT*")))
      (clo:make-variable
       client global-environment environment-symbol global-environment)
      (clo:make-variable
       client global-environment client-symbol client))
    (sb:ensure-asdf-system
     client environment "clostrophilia-package")
    (sb:ensure-asdf-system
     client environment "sicl-clos-package")
    (let ((e1-symbol
            (sb:intern-parcl-symbol
             client "SICL-CLOS" "ENSURE-GENERIC-FUNCTION-USING-CLASS"))
          (e2-symbol
            (sb:intern-parcl-symbol
             client "SICL-CLOS" "^ENSURE-GENERIC-FUNCTION-USING-CLASS")))
      (setf (clo:fdefinition client global-environment e2-symbol)
            (clo:fdefinition client (sb:e1 boot) e1-symbol)))
    (let ((e1-symbol
            (sb:intern-parcl-symbol
             client "SICL-CLOS" "ENSURE-CLASS-USING-CLASS"))
          (e2-symbol
            (sb:intern-parcl-symbol
             client "SICL-CLOS" "^ENSURE-CLASS-USING-CLASS")))
      (setf (clo:fdefinition client global-environment e2-symbol)
            (clo:fdefinition client (sb:e1 boot) e1-symbol))
      (sb:ensure-asdf-system
       client environment "sicl-clos-ensure-metaobject")))
  boot)
