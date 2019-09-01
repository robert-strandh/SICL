(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:compute-argument-count-instruction)
     lexical-environment)
  (let ((output (first (cleavir-ir:outputs instruction)))
        (successor (first (cleavir-ir:successors instruction))))
    (setf (gethash output lexical-environment)
          (length (gethash 'arguments lexical-environment)))
    successor))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:argument-instruction)
     lexical-environment)
  (let ((input-value (input-value (first (cleavir-ir:inputs instruction))
                                  lexical-environment))
        (output (first (cleavir-ir:outputs instruction)))
        (successor (first (cleavir-ir:successors instruction))))
    (setf (gethash output lexical-environment)
          (aref (gethash 'arguments lexical-environment) input-value))
    successor))
