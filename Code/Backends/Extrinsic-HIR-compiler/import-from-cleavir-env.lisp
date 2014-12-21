(cl:in-package #:sicl-extrinsic-hir-compiler)

(loop for symbol being each present-symbol in '#:cleavir-env
      when (and (fboundp symbol)
		(null (macro-function symbol)))
	do (setf (sicl-env:fdefinition symbol *environment*)
		 (fdefinition symbol))
      when (fboundp `(setf ,symbol))
	do (setf (sicl-env:fdefinition `(setf ,symbol) *environment*)
		 (fdefinition `(setf ,symbol))))
