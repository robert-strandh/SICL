(cl:in-package #:cleavir-ast-to-hir)

;;; in case boxing semantics change
(defun box-for-type (type inputs context)
  (make-instance 'cleavir-ir:box-instruction
    :element-type type
    :inputs inputs
    :outputs (results context)
    :successors (successors context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an AREF-AST

(defmethod compile-ast ((ast cleavir-ast:aref-ast) context)
  (assert-context ast context 1 1)
  (let* ((array-temp (make-temp))
	 (index-temp (make-temp))
	 (type (cleavir-ast:element-type ast))
	 (unboxed (if (cleavir-ast:boxed-p ast)
		      (results context)
		      ;; need an additional boxing step.
		      (list (make-temp))))
	 (succ (if (cleavir-ast:boxed-p ast)
		   (successors context)
		   (list (box-for-type type unboxed context)))))
    (compile-ast
     (cleavir-ast:array-ast ast)
     (context
      (list array-temp)
      (list (compile-ast
	     (cleavir-ast:index-ast ast)
	     (context (list index-temp)
		      (list (make-instance 'cleavir-ir:aref-instruction
			      :element-type type
			      :simple-p (cleavir-ast:simple-p ast)
			      :boxed-p (cleavir-ast:boxed-p ast)
			      :inputs (list array-temp index-temp)
			      :outputs unboxed
			      :successors succ))
		      (invocation context))))
      (invocation context)))))

(defun unbox-for-type (type input output successor)
  (make-instance 'cleavir-ir:unbox-instruction
    :element-type type
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an ASET-AST

(defmethod compile-ast ((ast cleavir-ast:aset-ast) context)
  (let* ((array-temp (make-temp))
	 (index-temp (make-temp))
	 (element-temp (make-temp))
	 (type (cleavir-ast:element-type ast))
	 (aset (make-instance 'cleavir-ir:aset-instruction
		 :element-type type
		 :simple-p (cleavir-ast:simple-p ast)
		 :boxed-p (cleavir-ast:boxed-p ast)
		 :inputs (list array-temp index-temp element-temp)
		 :outputs (results context)
		 :successors (successors context))))
    (compile-ast
     (cleavir-ast:array-ast ast)
     (context
      (list array-temp)
      (compile-ast
       (cleavir-ast:index-ast ast)
       (context
	(list index-temp)
	(compile-ast
	 (cleavir-ast:element-ast ast)
	 (if (cleavir-ast:boxed-p ast)
	     ;; simple case: no unbox required
	     (context (list element-temp)
		      (list aset)
		      (invocation context))
	     ;; if we have to unbox the new value first, compile
	     ;; the element-ast in a context where the successor
	     ;; is an unboxer and the output is a different temp.
	     (let ((boxed-temp (make-temp)))
	       (context
		(list boxed-temp)
		(list
		 (unbox-for-type type boxed-temp
				 element-temp aset))
		(invocation context)))))
	(invocation context)))
      (invocation context)))))
