(cl:in-package #:sicl-cons)

(defmacro remf (&environment environment place indicator)
  (let* ((global-env (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-env)))
    (remf-expander client environment place indicator)))
