(cl:in-package #:cleavir-ast-to-hir)

(defmethod acclimation:report-condition
    ((condition miscontext) stream (language acclimation:english))
  (with-accessors ((ast ast) (expected-results ast-results)
                   (expected-successors ast-successors)
                   (context miscontext-context))
      condition
    ;; signaling an error in an error report may not be good.
    (assert (or expected-results expected-successors))
    (format stream
            "Error during AST-TO-HIR:~@
             Found ~a with ~@[~d results ~]~@[~d successors~] in context where ~@[~d results ~]~@[~d successors ~]were expected.~@
             This is probably caused by a bug in GENERATE-AST."
            ast
            (if expected-results (length (results context)) nil)
            (if expected-successors
                (length (successors context))
                nil)
            expected-results expected-successors)))
