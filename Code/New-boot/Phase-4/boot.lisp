(cl:in-package #:sicl-new-boot-phase-4)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun boot (boot)
  (format *trace-output* "**************** Phase 4~%")
  (let* (#+sbcl(sb-ext:*evaluator-mode* :interpret)
         (client (make-instance 'client))
         (environment (create-environment client))
         (global-environment
           (trucler:global-environment client environment))
         (env:*client* client)
         (env:*environment* global-environment))
    (setf (sb:e4 boot) global-environment)
    (reinitialize-instance client
      :environment global-environment)
    (clo:make-variable
     client global-environment '*package* (find-package '#:common-lisp-user))
    (sb:define-package-functions client global-environment)
    (sb:define-backquote-macros client global-environment)
    (import-from-host client global-environment)
    (setf (clo:fdefinition client global-environment 'funcall)
          (lambda (function-or-name &rest arguments)
            (apply #'funcall
                   (if (functionp function-or-name)
                       function-or-name
                       (clo:fdefinition
                        client global-environment function-or-name))
                   arguments)))
    (sb:import-khazern client global-environment)
    (sb:fill-environment client global-environment)
    (sb:define-client-and-environment-variables client global-environment)
    (sb:define-environment-functions client global-environment)
    (sb:define-clostrophilia-find-method-combination-template
        client global-environment)
    (sb:ensure-asdf-system client environment "clostrophilia-slot-value-etc"))
  boot)
