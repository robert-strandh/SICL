(cl:in-package #:cleavir-ast-interpreter)

(defgeneric interpret-ast (ast static-env dynamic-env))

(defun interpret (ast)
  (let ((static-env (list (make-hash-table :test #'eq)))
	(dynamic-env '()))
    (interpret-ast ast static-env dynamic-env)))
