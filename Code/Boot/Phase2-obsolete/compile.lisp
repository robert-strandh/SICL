(cl:in-package #:sicl-boot-phase2)

(defun compile (dummy lambda-expression)
  (declare (ignore dummy lambda-expression))
  (sicl-boot-phase1:allocate-built-in-instance (find-bridge-class 'function)))
