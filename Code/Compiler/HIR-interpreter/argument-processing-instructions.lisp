(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:compute-argument-count-instruction)
     lexical-environment)
  (let ((output (first (cleavir-ir:outputs instruction)))
        (successor (first (cleavir-ir:successors instruction))))
    (setf (lexical-value output lexical-environment)
          (length (lexical-value 'arguments lexical-environment)))
    successor))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:argument-instruction)
     lexical-environment)
  (let ((input-value (input-value (first (cleavir-ir:inputs instruction))
                                  lexical-environment))
        (output (first (cleavir-ir:outputs instruction)))
        (successor (first (cleavir-ir:successors instruction))))
    (setf (lexical-value output lexical-environment)
          (aref (lexical-value 'arguments lexical-environment) input-value))
    successor))
