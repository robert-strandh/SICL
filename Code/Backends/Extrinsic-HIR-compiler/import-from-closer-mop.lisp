(cl:in-package #:sicl-extrinsic-hir-compiler)

(loop for symbol being each external-symbol in '#:closer-mop
      do (when (and (not (eq (symbol-package symbol)
			     (find-package '#:common-lisp)))
		    (find-symbol (symbol-name symbol) '#:sicl-clos)
		    (fboundp symbol)
		    (null (macro-function symbol)))
	   (setf (sicl-genv:fdefinition
		  (find-symbol (symbol-name symbol) '#:sicl-clos)
		  *environment*)
		 (fdefinition symbol))))
