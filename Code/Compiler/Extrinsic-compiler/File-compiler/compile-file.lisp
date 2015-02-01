(cl:in-package #:sicl-extrinsic-file-compiler)

(defun ast-from-stream (stream environment)
  (cleavir-ast:make-progn-ast
   (loop with eof = (list nil)
	 for form = (sicl-reader:read stream nil eof)
	 until (eq form eof)
	 collect (cleavir-generate-ast:generate-ast form environment))))

(defun compile-stream (stream environment)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile-file)
	 (ast (ast-from-stream stream environment))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast)))
    hir))

(defun compile-file (filename environment)
  (with-open-file (stream filename :direction :input)
    (compile-stream stream environment)))
