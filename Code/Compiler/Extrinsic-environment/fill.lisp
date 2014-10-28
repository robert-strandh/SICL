(cl:in-package #:sicl-extrinsic-environment)

(loop for symbol being each external-symbol in '#:common-lisp
      when (special-operator-p symbol)
	do (setf (sicl-env:special-operator symbol *environment*) t))

(setf (sicl-env:constant-variable t *environment*) t)
(setf (sicl-env:constant-variable nil *environment*) nil)

(setf (sicl-env:macro-function 'defmacro *environment*)
      (compile nil
	       (sicl-code-utilities:parse-macro
		'defmacro
		'(name lambda-list &body body)
		'(`(eval-when (:compile-toplevel :load-toplevel :execute)
		     (funcall #'(setf macro-function)
			      (function ,(sicl-code-utilities:parse-macro
					  name
					  lambda-list
					  body))
			      ',name))))))
