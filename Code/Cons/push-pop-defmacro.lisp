(cl:in-package #:sicl-cons)

(defmacro push (item place &environment environment)
  (let* ((global-env (env:global-environment environment)))
    (push-expander environment item place)))

(defmacro pop (place &environment environment)
  (let* ((global-env (env:global-environment environment)))
    (pop-expander environment place)))
