(cl:in-package #:cleavir-load-time-value-hoisting)

(defmethod compile-form (form system)
  (cleavir-ast-to-hir:ast-to-hir
   (cleavir-generate-ast:generate-ast form *compilation-environment* system)))

(defun hoist-load-time-values (hir system &key compilation-environment)
  (let ((*compilation-environment* compilation-environment))
    (with-constructor-tables
      (scan-hir hir system)
      (hoist-toplevel-hir hir system))))
