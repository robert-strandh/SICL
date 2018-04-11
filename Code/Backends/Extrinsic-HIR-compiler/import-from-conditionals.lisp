(cl:in-package #:sicl-extrinsic-hir-compiler)

(loop for symbol being each present-symbol in '#:sicl-conditionals
      when (and (fboundp symbol)
		(null (macro-function symbol)))
	do (setf (sicl-genv:fdefinition symbol *environment*)
		 (fdefinition symbol))
      when (fboundp `(setf ,symbol))
	do (setf (sicl-genv:fdefinition `(setf ,symbol) *environment*)
		 (fdefinition `(setf ,symbol))))

	   
