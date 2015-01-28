(cl:in-package #:sicl-extrinsic-environment)

(defun add-special-operators (environment)
  (loop for symbol being each external-symbol in '#:common-lisp
	when (special-operator-p symbol)
	  do (setf (sicl-env:special-operator symbol environment) t)))

(defun add-nil-and-t (environment)
  (setf (sicl-env:constant-variable t environment) t)
  (setf (sicl-env:constant-variable nil environment) nil))

(defun import-from-host-common-lisp (environment)
  (add-special-operators environment)
  (add-nil-and-t environment))
  
