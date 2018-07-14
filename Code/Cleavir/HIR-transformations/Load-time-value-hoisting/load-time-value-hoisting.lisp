(cl:in-package #:cleavir-load-time-value-hoisting)

(defun hoist-load-time-values (hir system)
  (with-constructor-tables
    (scan-hir hir system)
    (hoist-toplevel-hir hir system)))
