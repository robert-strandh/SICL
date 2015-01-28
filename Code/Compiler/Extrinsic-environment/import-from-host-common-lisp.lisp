(cl:in-package #:sicl-extrinsic-environment)

(defun import-special-operators (environment)
  (loop for symbol being each external-symbol in '#:common-lisp
	when (special-operator-p symbol)
	  do (setf (sicl-env:special-operator symbol environment) t)))

(defun import-nil-and-t (environment)
  (setf (sicl-env:constant-variable t environment) t)
  (setf (sicl-env:constant-variable nil environment) nil))

(defun import-from-host-common-lisp (environment)
  (import-special-operators environment)
  (import-nil-and-t environment))
  
