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
    (sb:ensure-asdf-system client environment "clostrophilia-slot-value-etc")
    (sb:define-straddle-functions client global-environment (sb:e2 boot))
    (sb:ensure-asdf-system client environment "sicl-clos-ensure-metaobject")
    (sb:define-ecclesia-functions client (sb:e1 boot) global-environment)
    (sb:ensure-asdf-system
     client environment "clostrophilia-method-combination")
    (setf (clo:fdefinition client (sb:e2 boot) @sicl-clos:subtypep-1)
          (constantly t))
    (setf (clo:fdefinition
           client (sb:e2 boot) @clostrophilia:find-class-t)
          (lambda ()
            (clo:find-class client global-environment 't)))
    (sb:ensure-asdf-system
     client environment "clostrophilia-class-hierarchy"))
  boot)
