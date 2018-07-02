(cl:in-package #:cleavir-load-time-value-hoisting)

(defmethod compile-form (form client)
  (cleavir-ast-to-hir:ast-to-hir
   (cleavir-generate-ast:generate-ast form *compilation-environment* client)))

(defun hoist-load-time-values (hir client &key compilation-environment)
  (let ((*compilation-environment* compilation-environment))
    (with-fresh-tables (scan-hir hir client)
      (hoist-hir hir client))))
