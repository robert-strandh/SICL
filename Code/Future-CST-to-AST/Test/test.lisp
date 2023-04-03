(cl:in-package #:sicl-expression-to-ast)

(defun test (form)
  (let* ((clearcut:*client*
           (make-instance 'clearcut-implementation-s-expression:client))
         (client (make-instance 'client))
         (global-environment (make-instance 'environment))
         (environment (make-instance 'trucler-reference:environment
                        :global-environment global-environment)))
    (expression-to-ast client form environment)))
