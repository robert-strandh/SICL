(cl:in-package #:sicl-new-boot-phase-3)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun boot (boot)
  (format *trace-output* "**************** Phase 3~%")
  (let* ((client (make-instance 'client))
         (environment (create-environment client))
         (global-environment
           (trucler:global-environment client environment))
         (env:*client* client)
         (env:*environment* global-environment))
    (setf (sb:e3 boot) global-environment)
    (reinitialize-instance client
      :environment global-environment)
    (clo:make-variable
     client global-environment '*package* (find-package '#:common-lisp-user))
    (sb:define-package-functions client global-environment)
    (sb:define-backquote-macros client global-environment)
    (import-from-host client global-environment)
    (sb:import-khazern client global-environment)
    (sb:define-client-and-environment-variables client global-environment)
    (sb:define-environment-functions client global-environment)
    (sb:define-clostrophilia-find-method-combination-template
        client global-environment)
    (sb:define-straddle-functions client global-environment (sb:e2 boot)))
  boot)
