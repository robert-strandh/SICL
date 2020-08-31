(cl:in-package #:sicl-data-and-control-flow)

(defmacro shiftf (&environment environment &rest arguments)
  (let* ((global-env (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-env)))
    (shiftf-expander client environment arguments)))
