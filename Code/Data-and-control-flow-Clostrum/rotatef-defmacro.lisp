(cl:in-package #:sicl-data-and-control-flow)

(defmacro rotatef (&environment environment &rest places)
  (let* ((global-env (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-env)))
    (rotatef-expander client environment places)))
