(cl:in-package #:sicl-cons)

(defmacro push (item place &environment environment)
  (let* ((global-env (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-env)))
    (push-expander client environment item place)))

(defmacro pop (place &environment environment)
  (let* ((global-env (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-env)))
    (pop-expander client environment place)))
