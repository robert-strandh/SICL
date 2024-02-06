(cl:in-package #:sicl-environment)

(defun fdefinition (function-name)
  (clo:fdefinition *client* *environment* function-name))

(defun (setf fdefinition) (new-definition function-name)
  (setf (clo:fdefinition *client* *environment* function-name)
        new-definition))
