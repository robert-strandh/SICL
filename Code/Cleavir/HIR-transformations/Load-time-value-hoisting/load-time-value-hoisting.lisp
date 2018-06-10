(cl:in-package #:cleavir-load-time-value-hoisting)

;;; A hash table with one entry per load-time-value form and literal
;;; object.
(defvar *root-objects*)

;;; Given a HIR flowchart, return two values.  The first value is the HIR
;;; graph of a thunk that returns a simple vector of objects.  The second
;;; value is a hash table mapping from each root object to the
;;; corresponding index in the vector of objects.
(defun hoist-load-time-values (hir &key compilation-environment)
  (let ((*compilation-environment* compilation-environment)
        (*data-constructors* (make-hash-table :test #'eq))
        (*eq-table* (make-hash-table :test #'eq))
        (*eql-table* (make-hash-table :test #'eql))
        (*equal-table* (make-hash-table :test #'equal)))
    (scan-hir hir)
    (reconstruct *data-constructors*)))
