(in-package #:sicl-data-and-control-flow)

(defun get-setf-expansion (place &optional environment)
  (let ((global-env (or environment *global-environment*)))
    (sicl-env:get-setf-expansion place global-env)))
