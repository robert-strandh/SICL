(cl:in-package #:sicl-ast-evaluator)

(defun translate-code (cst environment)
  (let* ((client (client environment))
         (ast (cleavir-cst-to-ast:cst-to-ast
               client cst environment))
         (table (make-hash-table :test #'eq))
         (lexical-environment (list table)))
    (let ((*run-time-environment-name* (gensym)))
      `(lambda (,*run-time-environment-name*)
         (declare (ignorable ,*run-time-environment-name*))
         ,(translate-ast ast environment lexical-environment)))))
