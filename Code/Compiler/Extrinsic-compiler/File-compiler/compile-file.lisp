(cl:in-package #:sicl-extrinsic-file-compiler)

(defparameter *sicl* (make-instance 'sicl-target-sicl:sicl))

(defparameter *gnu-linux* (make-instance 'sicl-os-gnu-linux:gnu-linux))

(defparameter *x86-64* (make-instance 'cleavir-processor-x86-64:x86-64))

(defun ast-from-stream (stream environment system)
  (cleavir-ast:make-progn-ast
   (loop with eof = (list nil)
	 for cst = (eclector.concrete-syntax-tree:cst-read stream nil eof)
	 until (eq cst eof)
	 collect (cleavir-cst-to-ast:cst-to-ast cst environment system))))

(defun compile-stream (stream environment)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile-file)
	 (ast (ast-from-stream stream environment *sicl*)))
    (let* ((ast-bis (cleavir-ast-transformations:hoist-load-time-value ast))
	   (hir (cleavir-ast-to-hir:compile-toplevel ast-bis)))
      (cleavir-hir-transformations:hir-transformations
       hir
       *sicl*
       *x86-64*
       *gnu-linux*)
      ;; (cleavir-ir:hir-to-mir hir *sicl* *x86-64* *gnu-linux*)
      (values hir ast-bis))))

(defun compile-file (filename environment)
  (with-open-file (stream filename :direction :input)
    (compile-stream stream environment)))
