(cl:in-package #:sicl-evaluation-and-compilation)

(defun constantp (form &optional environment)
  (let* ((global-environment (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-environment)))
    (or (and (not (symbolp form))
             (not (consp form)))
        (keywordp form)
        (and (symbolp form)
             (nth-value 1 (sicl-environment:constant-variable
                           client global-environment form)))
        (and (consp form)
             (eq (car form) 'quote)))))
