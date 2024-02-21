(cl:in-package #:sicl-environment)

(defun define-constant (name initial-value &key (environment *environment*))
  (clo:make-constant *client* environment name initial-value))
