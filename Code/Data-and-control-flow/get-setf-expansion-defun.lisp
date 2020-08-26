(in-package #:sicl-data-and-control-flow)

;;; FIXME: This definition is clearly wrong, but it will go away in the future anyway.
(defun get-setf-expansion (place &optional environment)
  (let ((global-env (if (null environment)
                        (sicl-genv:global-environment)
                        (sicl-genv:global-environment environment))))
    (sicl-global-environment:get-setf-expansion place global-env)))
