(cl:in-package #:sicl-extrinsic-environment)

(defun import-from-sicl-conditionals (environment)
  (loop for symbol being each external-symbol in '#:sicl-conditionals
	when (and (fboundp symbol)
		  (not (special-operator-p symbol))
		  (null (macro-function symbol)))
	  do (setf (sicl-env:fdefinition
		    symbol
		    environment)
		   (fdefinition symbol))
	when (fboundp (list 'setf symbol))
	  do (setf (sicl-env:fdefinition
		    (list 'setf symbol)
		    environment)
		   (fdefinition (list 'setf symbol)))))
