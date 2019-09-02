(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:unwind-instruction)
     lexical-environment)
  (let ((input-value (input-value (first (cleavir-ir:inputs instruction))
                                  lexical-environment))
        (unwind-index (cleavir-ir:unwind-index instruction)))
    (throw input-value unwind-index)))
