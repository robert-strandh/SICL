(cl:in-package #:cleavir-ast-to-hir)

(defmethod acclimation:report-condition
    ((condition miscontext) stream (language acclimation:english))
  (with-accessors ((ast ast) (results ast-results)
		   (successors ast-successors)
		   (context miscontext-context))
      condition
    ;; signaling an error in an error report may not be good.
    (assert (or results successors))
    (format stream
	    "Error during AST-TO-HIR:~@
             Found ~a (which has ~@[~d results ~]~@[~d successors~]) in context where ~@[~d results ~]~@[~d successors ~]were expected.~@
             This is probably caused by a bug in GENERATE-AST."
	    ast results successors
	    (if results (results context) nil)
	    (if successors (length (successors context)) nil))))
