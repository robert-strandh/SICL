(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:characterp-instruction)
     lexical-environment)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (successors (cleavir-ir:successors instruction)))
    (if (characterp (input-value input lexical-environment))
        (first successors)
        (second successors))))
