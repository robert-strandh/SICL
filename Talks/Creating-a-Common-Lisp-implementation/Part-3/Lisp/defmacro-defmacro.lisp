(cl:in-package #:target-evaluation-and-compilation)

(defmacro defmacro (name lambda-list &body body)
  (cleavir-code-utilities:parse-macro name lambda-list body))
