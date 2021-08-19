(cl:in-package #:sicl-cons)

(defmacro remf (&environment environment place indicator)
  (let ((global-env (sicl-environment:global-environment environment)))
    (remf-expander environment place indicator)))
