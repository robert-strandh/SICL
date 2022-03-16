(cl:in-package #:sicl-environment)

(defun make-environment-for-file-compilation (run-time-environment)
  (make-instance 'compilation-environment
    :parent (make-instance 'evaluation-environment
              :parent run-time-environment)))
