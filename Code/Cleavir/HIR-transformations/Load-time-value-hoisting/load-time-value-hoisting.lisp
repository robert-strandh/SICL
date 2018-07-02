(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Given a HIR flowchart, return two values.  The first value is the HIR
;;; graph of a thunk that returns a simple vector of objects.  The second
;;; value is a hash table mapping from each root object to the
;;; corresponding index in the vector of objects.


(defun hoist-load-time-values (hir client &key compilation-environment)
  (let ((*compilation-environment* compilation-environment))
    (with-fresh-tables (scan-hir hir client)
      (hoist-data hir client))))
