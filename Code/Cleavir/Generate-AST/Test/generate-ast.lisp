(in-package #:cleavir-test-generate-ast)

(defgeneric same-p (ast1 ast2))

(defvar *table*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testing framework.

(defun same-ast-p (ast1 ast2)
  (let ((*table* (make-hash-table :test #'equal)))
    (same-p ast1 ast2)))

(defmethod same-p :around (ast1 ast2)
  (cond ((not (eq (class-of ast1) (class-of ast2)))
	 nil)
	((gethash (cons ast1 ast2) *table*)
	 t)
	(t
	 (setf (gethash (cons ast1 ast2) *table*) t)
	 (call-next-method ast1 ast2))))

(defmethod same-p ((ast1 cleavir-ast:constant-ast) ast2)
  (equalp (cleavir-ast:value ast1) (cleavir-ast:value ast2)))

(defmethod same-p ((ast1 cleavir-ast:lexical-ast) ast2)
  t)

(defmethod same-p ((ast1 cleavir-ast:symbol-value-ast) ast2)
  (eq (cleavir-ast:symbol ast1) (cleavir-ast:symbol ast2)))

(defmethod same-p ((ast1 cleavir-ast:set-symbol-value-ast) ast2)
  (and (eq (cleavir-ast:symbol ast1) (cleavir-ast:symbol ast2))
       (same-p (cleavir-ast:value-ast ast1) (cleavir-ast:value-ast ast2))))

(defmethod same-p ((ast1 cleavir-ast:fdefinition-ast) ast2)
  (equal (cleavir-ast:name ast1) (cleavir-ast:name ast2)))

(defmethod same-p ((ast1 cleavir-ast:call-ast) ast2)
  (and (same-p (cleavir-ast:callee-ast ast1) (cleavir-ast:callee-ast ast2))
       (every #'same-p
	      (cleavir-ast:argument-asts ast1)
	      (cleavir-ast:argument-asts ast2))))

(defmethod same-p ((ast1 cleavir-ast:function-ast) ast2)
  (flet ((same-lambda-list-entry (e1 e2)
	   (cond ((and (eq e1 e2)
		       (member e1 lambda-list-keywords)
		       (member e2 lambda-list-keywords))
		  t)
		 ((or (member e1 lambda-list-keywords)
		      (member e2 lambda-list-keywords))
		  nil)
		 ((and (listp e1)
		       (listp e2)
		       (= (length e1) (length e2))
		       (every #'same-p e1 e2))
		  t)
		 (t
		  (same-p e1 e2)))))
    (and (every #'same-lambda-list-entry
		(cleavir-ast:lambda-list ast1)
		(cleavir-ast:lambda-list ast2))
	 (same-p (cleavir-ast:body-ast ast1)
		 (cleavir-ast:body-ast ast2)))))

(defmethod same-p ((ast1 cleavir-ast:progn-ast) ast2)
  (every #'same-p
	 (cleavir-ast:form-asts ast1)
	 (cleavir-ast:form-asts ast2)))

(defmethod same-p ((ast1 cleavir-ast:block-ast) ast2)
  (same-p (cleavir-ast:body-ast ast1) (cleavir-ast:body-ast ast2)))

(defmethod same-p ((ast1 cleavir-ast:return-from-ast) ast2)
  (and (same-p (cleavir-ast:block-ast ast1) (cleavir-ast:block-ast ast2))
       (same-p (cleavir-ast:form-ast ast1) (cleavir-ast:form-ast ast1))))

(defmethod same-p ((ast1 cleavir-ast:setq-ast) ast2)
  (and (same-p (cleavir-ast:lhs-ast ast1) (cleavir-ast:lhs-ast ast2))
       (same-p (cleavir-ast:value-ast ast1) (cleavir-ast:value-ast ast2))))

(defmethod same-p ((ast1 cleavir-ast:tag-ast) ast2)
  (eq (cleavir-ast:name ast1) (cleavir-ast:name ast2)))

(defmethod same-p ((ast1 cleavir-ast:tagbody-ast) ast2)
  (every #'same-p
	 (cleavir-ast:item-asts ast1)
	 (cleavir-ast:item-asts ast2)))

(defmethod same-p ((ast1 cleavir-ast:go-ast) ast2)
  (same-p (cleavir-ast:tag-ast ast1) (cleavir-ast:tag-ast ast2)))

(defmethod same-p ((ast1 cleavir-ast:the-ast) ast2)
  (and (eq (cleavir-ast:check-p ast1) (cleavir-ast:check-p ast2))
       (equal (cleavir-ast:type-specifiers ast1)
	      (cleavir-ast:type-specifiers ast2))
       (same-p (cleavir-ast:form-ast ast1)
	       (cleavir-ast:form-ast ast2))))

(defmethod same-p ((ast1 cleavir-ast:typeq-ast) ast2)
  (and (equal (cleavir-ast:type-specifier ast1)
	      (cleavir-ast:type-specifier ast2))
       (same-p (cleavir-ast:form-ast ast1) (cleavir-ast:form-ast ast2))))

(defmethod same-p ((ast1 cleavir-ast:load-time-value-ast) ast2)
  (and (eq (cleavir-ast:read-only-p ast1) (cleavir-ast:read-only-p ast2))
       (same-p (cleavir-ast:form-ast ast1) (cleavir-ast:form-ast ast2))))

(defmethod same-p ((ast1 cleavir-ast:if-ast) ast2)
  (and (same-p (cleavir-ast:test-ast ast1) (cleavir-ast:test-ast ast2))
       (same-p (cleavir-ast:then-ast ast1) (cleavir-ast:then-ast ast2))
       (same-p (cleavir-ast:else-ast ast1) (cleavir-ast:else-ast ast2))))

(defmethod same-p ((ast1 cleavir-ast:eq-ast) ast2)
  (and (same-p (cleavir-ast:arg1-ast ast1) (cleavir-ast:arg1-ast ast2))
       (same-p (cleavir-ast:arg2-ast ast1) (cleavir-ast:arg2-ast ast2))))

(defparameter *e* (make-instance 'bogus-environment))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* cleavir-io:*io-readtable*))

(defun test (form value)
  (let* ((ast (cleavir-generate-ast:generate-ast form *e*))
	 (v (cleavir-ast-interpreter:interpret ast)))
    (assert (equalp v value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests

(defun test-constant-ast ()
  (test 234 234))

(defun test-lexical-ast ()
  (test '(let ((x 10)) x)
	10))
