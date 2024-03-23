(cl:in-package #:sicl-new-boot)

(defun repl (client global-environment)
  (let* ((environment
           (make-instance 'trucler-reference:environment
             :global-environment global-environment))
         (eclector.base:*client* client)
         (abp:*builder* (cb::make-builder client environment)))
    (loop for prompt = (progn (princ ">>> ") (finish-output))
          for form = (eclector.reader:read)
          for cst = (cst:cst-from-expression form)
          for values
            = (multiple-value-list (eval-cst client cst environment))
          do (print values)
             (terpri)
             (finish-output))))
