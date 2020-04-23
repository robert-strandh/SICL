(in-package #:sicl-data-and-control-flow)

;;; FIXME: This definition is wrong because it does not allow for the
;;; place to be macroexpanded in the lexical environment.
(defun get-setf-expansion (place &optional environment)
  (let ((global-env (if (null environment)
                        (sicl-genv:global-environment)
                        (sicl-genv:global-environment environment))))
    (sicl-global-environment:get-setf-expansion place global-env)))
