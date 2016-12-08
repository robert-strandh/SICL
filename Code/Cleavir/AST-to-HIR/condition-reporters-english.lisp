(cl:in-package #:cleavir-ast-to-hir)

(defmethod acclimation:report-condition
    ((condition miscontext) stream (language acclimation:english))
  ;; signaling an error in an error report may not be good.
  (assert (or (ast-results condition) (ast-successors condition)))
  (format stream
	  "Error during AST-TO-HIR:~@
           Found ~a (which has ~@[~d results ~]~@[~d successors~]) in context where ~@[~d results ~]~@[~d successors ~] were expected.
           This is probably caused by a bug in GENERATE-AST."
	  (ast condition)
	  (ast-results condition)
	  (ast-successors condition)
	  (if (ast-results condition) (results context) nil)
	  (if (ast-successors condition)
	      (successors context) nil)))
