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

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:char-code-instruction)
     lexical-environment)
  (let* ((input-value (input-value (first (cleavir-ir:inputs instruction))
                                   lexical-environment))
         (output (first (cleavir-ir:outputs instruction)))
         (successor (first (cleavir-ir:successors instruction))))
    (setf (lexical-value output lexical-environment)
          (char-code input-value))
    successor))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:code-char-instruction)
     lexical-environment)
  (let* ((input-value (input-value (first (cleavir-ir:inputs instruction))
                                   lexical-environment))
         (output (first (cleavir-ir:outputs instruction)))
         (successor (first (cleavir-ir:successors instruction))))
    (setf (lexical-value output lexical-environment)
          (code-char input-value))
    successor))
