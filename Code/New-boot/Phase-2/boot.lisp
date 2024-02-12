(cl:in-package #:sicl-new-boot-phase-2)

(defun boot (boot)
  (format *trace-output* "**************** Phase 2~%")
  (let* ((client (make-instance 'client))
         (environment (create-environment client))
         (global-environment
           (trucler:global-environment client environment))
         (env:*client* client)
         (env:*environment* global-environment))
    (setf (sb:e2 boot) global-environment
          (sb:c2 boot) client)
    (reinitialize-instance client
      :environment global-environment)
    (clo:make-variable
     client global-environment '*package* (find-package '#:common-lisp-user))
    (sb:define-package-functions client global-environment)
    (sb:define-backquote-macros client global-environment)
    (import-from-host client global-environment)
    (sb:import-khazern client global-environment)
    (sb:define-environment-functions client global-environment)
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
    (define-ensure-method-combination-template
        client (sb:e1 boot) global-environment)
    (define-^ensure-method-combination
        client (sb:e1 boot) global-environment)
    (define-find-method-combination-template client global-environment)
    (define-ensure-generic-function client (sb:e1 boot) global-environment)
    (define-ensure-class client (sb:e1 boot) global-environment)
    ;; We are loading this system into E1, but *ENVIRONMENT* is set to
    ;; E2, so when the extrinsic environment functions in E1 are
    ;; invoked, they will call the Clostrum functions with E2, thereby
    ;; defining the macros in E2.
    (let ((environment (make-instance 'trucler-reference:environment
                         :global-environment (sb:e1 boot))))
      (sb:ensure-asdf-system
       (sb:c1 boot) environment "common-macro-definitions-shared"))
    (define-ecclesia-functions client (sb:e1 boot) global-environment)
    (sb:ensure-asdf-system
     client environment "clostrophilia-method-combination"))
  boot)
