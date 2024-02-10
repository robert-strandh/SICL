(cl:in-package #:sicl-environment)

(defun define-constant (name initial-value)
  (clo:make-constant *client* *environment* name initial-value))
