(cl:in-package #:sicl-extrinsic-file-compiler)

(defparameter *sicl* (make-instance 'sicl-target-sicl:sicl))

(defparameter *gnu-linux* (make-instance 'sicl-os-gnu-linux:gnu-linux))

(defparameter *x86-64* (make-instance 'cleavir-processor-x86-64:x86-64))

(defun ast-from-stream (stream environment)
  (cleavir-ast:make-progn-ast
   (loop with eof = (list nil)
	 for form = (sicl-reader:read stream nil eof)
	 until (eq form eof)
	 collect (cleavir-generate-ast:generate-ast form environment))))

(defun hir-transformations (initial-instruction)
  (cleavir-hir-transformations:process-fdefinitions
   initial-instruction *sicl* *x86-64* *gnu-linux*)
  (cleavir-hir-transformations:type-inference initial-instruction)
  (cleavir-hir-transformations:eliminate-typeq initial-instruction))

(defun compile-stream (stream environment)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile-file)
	 (ast (ast-from-stream stream environment))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast)))
    (cleavir-hir-transformations:hir-transformations
     hir
     *sicl*
     *x86-64*
     *gnu-linux*)
    (cleavir-ir:hir-to-mir hir *sicl* *x86-64* *gnu-linux*)
    hir))

(defun compile-file (filename environment)
  (with-open-file (stream filename :direction :input)
    (compile-stream stream environment)))

(defmethod cleavir-generate-ast:convert-constant-to-immediate
    (constant (environment environment))
  (cond ((and (typep constant 'integer)
	      (<= #.(- (expt 2 30)) constant #.(1- (expt 2 30))))
	 (* 2 constant))
	((typep constant 'character)
	 ;; FIXME: Currently, we depend on the host having the same
	 ;; character encoding as the target.
	 (+ #b11 (* 4 (char-code constant))))
	(t
	 nil)))
