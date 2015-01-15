(cl:in-package #:sicl-extrinsic-hir-compiler)

(loop for symbol being each external-symbol in '#:cleavir-code-utilities
      when (and (fboundp symbol)
		(not (special-operator-p symbol))
		(null (macro-function symbol)))
	do (setf (sicl-env:fdefinition
		  symbol
		  *environment*)
		 (fdefinition symbol))
	   (setf (sicl-env:fdefinition
		  (find-symbol (symbol-name symbol)
			       '#:host-common-lisp)
		  *environment*)
		 (fdefinition symbol))
      when (fboundp (list 'setf symbol))
	do (setf (sicl-env:fdefinition
		  (list 'setf symbol)
		  *environment*)
		 (fdefinition (list 'setf symbol)))
	   (setf (sicl-env:fdefinition
		  (list 'setf
			(find-symbol (symbol-name symbol)
				     '#:host-common-lisp))
		  *environment*)
		 (fdefinition (list 'setf symbol))))
