(cl:in-package #:sicl-extrinsic-file-compiler)

(defparameter *sicl* (make-instance 'sicl-target-sicl:sicl))

(defparameter *gnu-linux* (make-instance 'sicl-os-gnu-linux:gnu-linux))

(defparameter *x86-64* (make-instance 'sicl-x86-64:x86-64))

(defun ast-from-stream (stream environment)
  (cleavir-ast:make-progn-ast
   (loop with eof = (list nil)
	 for form = (sicl-reader:read stream nil eof)
	 until (eq form eof)
	 collect (cleavir-generate-ast:generate-ast form environment))))

(defun compile-stream (stream environment)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile-file)
	 (ast (ast-from-stream stream environment))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast))
	 (hir-bis (cleavir-hir-transformations:hir-transformations
		   hir
		   *sicl*
		   *x86-64*
		   *gnu-linux*)))
    hir-bis))

(defun compile-file (filename environment)
  (with-open-file (stream filename :direction :input)
    (compile-stream stream environment)))
