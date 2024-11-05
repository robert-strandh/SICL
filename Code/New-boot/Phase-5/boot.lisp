(cl:in-package #:sicl-new-boot-phase-5)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun boot (boot)
  (format *trace-output* "**************** Phase 5~%")
  (let* ((e3 (sb:e3 boot))
         (e4 (sb:e4 boot))
         (environment
           (make-instance 'trucler-reference:environment
             :global-environment e4))
         (client (make-instance 'client))
         (env:*client* client)
         (env:*environment* e4))
    (reinitialize-instance client :environment e4)
    (finalize-inheritance client e3 e4)))
