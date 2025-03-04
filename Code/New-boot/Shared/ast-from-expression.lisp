(cl:in-package #:sicl-new-boot)

(defun ast-from-expression (expression boot)
  (let* ((*boot* boot)
         (global-environment (e4 boot))
         (package (find-package '#:sicl-new-boot-phase-4))
         (client-symbol (find-symbol "CLIENT" package))
         (client-object (make-instance client-symbol
                          :environment global-environment))
         (environment
           (make-instance 'trucler-reference:environment
             :global-environment global-environment))
         (eclector.base:*client* client-object)
         (abp:*builder* (cb::make-builder client-object environment))
         (cst (cst:cst-from-expression expression))
         (cmd:*client* client))
    (cb:cst-to-ast client cst environment)))
