(cl:in-package #:sicl-direct-extrinsic-compiler)

;;; Find variables that are live immediately after a call instruction.
(defun find-live-after-call (initial-instruction)
  (let ((liveness (cleavir-liveness:liveness
		   initial-instruction
		   #'cleavir-ir:successors
		   #'cleavir-ir:inputs 
		   #'cleavir-ir:outputs))
	(table (make-hash-table :test #'eq))
	(result '()))
    (labels ((traverse (insn)
	       (unless (gethash insn table)
		 (setf (gethash insn table) t)
		 (when (typep insn 'cleavir-ir:funcall-instruction)
		   (setf result
			 (append (cleavir-liveness:live-after liveness insn)
				 result)))
		 (traverse (cleavir-ir:successors insn)))))
      (traverse initial-instruction))))

(defun compile-function-form (form processor os)
  (let* ((environment sicl-extrinsic-environment:*environment*)
	 (sicl-env:*global-environment* environment)
	 (sicl (make-instance 'sicl-target-sicl:sicl))
	 (ast (cleavir-generate-ast:generate-ast form environment))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast))
	 (mir (cleavir-ir:hir-to-mir hir sicl processor os)))
    mir))
