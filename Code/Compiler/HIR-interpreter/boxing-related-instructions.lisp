(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:box-instruction)
     lexical-environment)
  (let* ((input-value (input-value (first (cleavir-ir:inputs instruction))
                                   lexical-environment))
         (output (first (cleavir-ir:outputs instruction)))
         (successor (first (cleavir-ir:successors instruction))))
    (setf (lexical-value output lexical-environment)
          input-value)
    successor))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:unbox-instruction)
     lexical-environment)
  (let* ((input-value (input-value (first (cleavir-ir:inputs instruction))
                                   lexical-environment))
         (output (first (cleavir-ir:outputs instruction)))
         (successor (first (cleavir-ir:successors instruction))))
    (setf (lexical-value output lexical-environment)
          input-value)
    successor))
