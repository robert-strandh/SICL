(cl:in-package #:sicl-extrinsic-file-compiler)

(defun ast-from-stream (stream)
  (apply #'cleavir-ast:make-progn-ast
	 (loop with eof = (list nil)
	       for form = (sicl-reader:read stream nil eof)
	       until (eq form eof)
	       collect form)))

(defun compile-stream (stream)
  (let* ((ast (ast-from-stream stream))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast)))
    hir))
