(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FIND-CLASS.

(defun find-class (symbol &optional (errorp t) environment)
  (let* ((global-environment (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-environment))
         (class (sicl-environment:find-class client global-environment symbol)))
    (if (and (null class) errorp)
	(error 'sicl-clos:no-such-class-name :name symbol)
	class)))
