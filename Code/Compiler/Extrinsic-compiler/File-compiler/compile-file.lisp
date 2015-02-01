(cl:in-package #:sicl-extrinsic-file-compiler)

(defun ast-from-stream (stream)
  (apply #'cleavir-ast:make-progn-ast
	 (loop with eof = (list nil)
	       for form = (sicl-reader:read stream nil eof)
	       until (eq form eof)
	       collect form)))
