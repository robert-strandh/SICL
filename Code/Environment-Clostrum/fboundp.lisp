(cl:in-package #:sicl-environment)

(defun fboundp (function-name)
  (clo:fboundp *client* *environment* function-name))
