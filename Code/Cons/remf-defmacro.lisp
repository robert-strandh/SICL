(cl:in-package #:sicl-cons)

(defmacro remf (&environment environment place indicator)
  (let ((global-env (env:global-environment environment)))
    (remf-expander environment place indicator)))
