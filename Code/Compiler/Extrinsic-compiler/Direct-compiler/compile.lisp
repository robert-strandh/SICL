(cl:in-package #:sicl-direct-extrinsic-compiler)

(defun compile-function-form (form)
  (let* ((environment sicl-extrinsic-environment:*environment*)
	 (sicl-env:*global-environment* environment)
	 (ast (cleavir-generate-ast:generate-ast form environment)))
    ast))
