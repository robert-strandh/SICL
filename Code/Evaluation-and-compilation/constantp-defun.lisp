(cl:in-package #:sicl-evaluation-and-compilation)

(let* ((global-environment (env:global-environment environment))
       (client (env:client global-environment))
       (constant-variable-function (fdefinition 'env:constant-variable)))
  (defun constantp (form &optional environment)
    (or (and (not (symbolp form))
             (not (consp form)))
        (keywordp form)
        (and (symbolp form)
             (nth-value 1 (funcall constant-variable-function
                                   client global-environment form)))
        (and (consp form)
             (eq (car form) 'quote)))))
