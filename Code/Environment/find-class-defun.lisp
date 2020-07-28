(cl:in-package #:sicl-standard-environment-functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FIND-CLASS.

(defun find-class (symbol &optional (errorp t) environment)
  (let* ((global-environment (sicl-genv:global-environment environment))
         (class (sicl-genv:find-class symbol global-environment)))
    (if (and (null class) errorp)
	(error 'sicl-clos:no-such-class-name :name symbol)
	class)))
