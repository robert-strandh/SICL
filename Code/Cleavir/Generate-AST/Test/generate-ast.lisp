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
  (declare (cl:ignore table))
  (equalp (cleavir-ast:value ast1) (cleavir-ast:value ast2)))

(defmethod same-p ((ast1 cleavir-ast:lexical-ast) ast2 table)
  (declare (cl:ignorable ast1 ast2 table))
  t)

(defmethod same-p ((ast1 cleavir-ast:symbol-value-ast) ast2 table)
  (declare (cl:ignore table))
  (eq (cleavir-ast:symbol ast1) (cleavir-ast:symbol ast2)))

(defmethod same-p ((ast1 cleavir-ast:set-symbol-value-ast) ast2 table)
  (declare (cl:ignore table))
  (and (eq (cleavir-ast:symbol ast1) (cleavir-ast:symbol ast2))
       (same-p (cleavir-ast:value-ast ast1) (cleavir-ast:value-ast ast2))))

(defmethod same-p ((ast1 cleavir-ast:fdefinition-ast) ast2 table)
  (declare (cl:ignore table))
  (equal (cleavir-ast:name ast1) (cleavir-ast:name ast2)))

(defmethod same-p ((ast1 cleavir-ast:call-ast) ast2 table)
  (and (same-p (cleavir-ast:callee-ast ast1) (cleavir-ast:callee-ast ast2))
       (every (lambda (a1 a2) (same-p a1 a2 table))
	      (cleavir-ast:argument-asts ast1)
	      (cleavir-ast:argument-asts ast2))))

(defmethod same-p ((ast1 cleavir-ast:function-ast) ast2 table)
  (flet ((same-lambda-list-entry (e1 e2)
	   (or (and (eq e1 '&optional)
		    (eq e2 '&optional))
	       (and (eq e1 '&key)
		    (eq e2 '&key))
	       (and (eq e1 '&allow-other-keys)
		    (eq e2 '&allow-other-keys))
	       (and (listp e1)
		    (listp e2)
		    (= (length e1) (length e2))
		    (every (lambda (a1 a2) (same-p a1 a2 table)) e1 e2))
	       (same-p e1 e2 table))))
    (and (every #'same-lambda-list-entry
		(cleavir-ast:lambda-list ast1)
		(cleavir-ast:lambda-list ast2))
	 (same-p (cleavir-ast:body-ast ast1)
		 (cleavir-ast:body-ast ast2)))))

(defmethod same-p ((ast1 cleavir-ast:progn-ast) ast2 table)
  (every (lambda (a1 a2) (same-p a1 a2 table))
	 (cleavir-ast:form-asts ast1)
	 (cleavir-ast:form-asts ast2)))

(defmethod same-p ((ast1 cleavir-ast:block-ast) ast2 table)
  (same-p (cleavir-ast:body-ast ast1) (cleavir-ast:body-ast ast2) table))

(defmethod same-p ((ast1 cleavir-ast:return-from-ast) ast2 table)
  (and (same-p (cleavir-ast:block-ast ast1) (cleavir-ast:block-ast ast2) table)
       (same-p (cleavir-ast:form-ast ast1) (cleavir-ast:form-ast ast1) table)))

(defmethod same-p ((ast1 cleavir-ast:setq-ast) ast2 table)
  (and (same-p (cleavir-ast:lhs-ast ast1) (cleavir-ast:lhs-ast ast2) table)
       (same-p (cleavir-ast:value-ast ast1) (cleavir-ast:value-ast ast2) table)))

(defmethod same-p ((ast1 cleavir-ast:tag-ast) ast2 table)
  (declare (ignore table))
  (eq (cleavir-ast:name ast1) (cleavir-ast:name ast2)))

(defmethod same-p ((ast1 cleavir-ast:tagbody-ast) ast2 table)
  (every (lambda (a1 a2) (same-p a1 a2 table))
	 (cleavir-ast:item-asts ast1)
	 (cleavir-ast:item-asts ast2)))
	 
