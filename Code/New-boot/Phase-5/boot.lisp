(cl:in-package #:sicl-new-boot-phase-5)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun wrap-environment (environment)
  (make-instance 'trucler-reference:environment
    :global-environment environment))

(defun boot (boot)
  (format *trace-output* "**************** Phase 5~%")
  (let* ((e3 (sb:e3 boot))
         (e4 (sb:e4 boot))
         (c3 (make-instance 'client))
         (c4 (make-instance 'client))
         (env:*client* c4)
         (env:*environment* e4)
         (client c4))
    (reinitialize-instance c3 :environment e3)
    (reinitialize-instance c4 :environment e4)
    (sb:ensure-asdf-system
     c3 (wrap-environment e3) "sicl-clos-satiation")
    (satiate-metaobject-functions-1 c3 e3 e4)
    (fix-forward-referring-functions client e3 e4)
    (fix-backward-referring-functions client e3 e4)))
