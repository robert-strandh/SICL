(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-test-generate-ast
  (:use #:common-lisp))

(in-package #:cleavir-test-generate-ast)

(defgeneric same-p (ast1 ast2 table))

(defun same-ast-p (ast1 ast2)
  (same-p ast1 ast2 '()))

(defmethod same-p :around (ast1 ast2 table)
  (cond ((not (eq (class-of ast1) (class-of ast2)))
	 nil)
	((member (cons ast1 ast2) table :test #'equal)
	 t)
	(t
	 (call-next-method ast1 ast2 (cons (cons ast1 ast2) table)))))

(defmethod same-p ((ast1 cleavir-ast:constant-ast) ast2 table)
  (equalp (cleavir-ast:value ast1) (cleavir-ast:value ast2)))

(defmethod same-p ((ast1 cleavir-ast:lexical-ast) ast2 table)
  (equal (cleavir-ast:name ast1) (cleavir-ast:name ast2)))
