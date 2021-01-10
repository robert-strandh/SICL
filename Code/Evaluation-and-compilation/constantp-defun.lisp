(cl:in-package #:sicl-evaluation-and-compilation)

(let* ((global-environment (sicl-environment:global-environment environment))
       (client (sicl-environment:client global-environment))
       (constant-variable-function (fdefinition 'sicl-environment:constant-variable)))
  (defun constantp (form &optional environment)
    (or (and (not (symbolp form))
             (not (consp form)))
        (keywordp form)
        (and (symbolp form)
             (nth-value 1 (funcall constant-variable-function
                                   client global-environment form)))
        (and (consp form)
             (eq (car form) 'quote)))))
