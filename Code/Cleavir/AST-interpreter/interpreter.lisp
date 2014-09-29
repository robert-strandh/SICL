(cl:in-package #:cleavir-ast-interpreter)

(defgeneric interpret-ast (ast static-env dynamic-env))

(defun interpret (ast)
  (let ((static-env (list (make-hash-table :test #'eq)))
	(dynamic-env '()))
    (interpret-ast ast static-env dynamic-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility function.

(defun interpret-sequence (sequence static-env dynamic-env)
  (loop for ast in sequence
	for value = (interpret-ast ast static-env dynamic-env)
	finally (return value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on INTERPRET-AST.

(defmethod interpret-ast ((ast cleavir-ast:constant-ast)
			  static-env dynamic-env)
  (declare (ignore static-env dynamic-env))
  (cleavir-ast:value ast))
