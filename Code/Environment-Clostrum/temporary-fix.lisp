(cl:in-package #:sicl-environment)

(defmethod (setf fdefinition) :after
    (new-definition client (environment base-run-time-environment) name)
  (setf (car (function-cell (client environment) environment name))
        new-definition))
