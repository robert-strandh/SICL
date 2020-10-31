(cl:in-package #:sicl-data-and-control-flow)

(defmacro psetf (&environment environment &rest pairs)
  (let* ((global-env (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-env)))
    (psetf-expander client environment pairs)))
