(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:car-instruction)
     lexical-environment)
  (let* ((input-value (input-value (first (cleavir-ir:inputs instruction))
                                   lexical-environment))
         (output (first (cleavir-ir:outputs instruction)))
         (successor (first (cleavir-ir:successors instruction))))
    (setf (lexical-value output lexical-environment)
          (car input-value))
    successor))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:cdr-instruction)
     lexical-environment)
  (let* ((input-value (input-value (first (cleavir-ir:inputs instruction))
                                   lexical-environment))
         (output (first (cleavir-ir:outputs instruction)))
         (successor (first (cleavir-ir:successors instruction))))
    (setf (lexical-value output lexical-environment)
          (cdr input-value))
    successor))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:rplaca-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (successor (first (cleavir-ir:successors instruction))))
    (rplaca (input-value (first inputs) lexical-environment)
            (input-value (second inputs) lexical-environment))
    successor))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:rplacd-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (successor (first (cleavir-ir:successors instruction))))
    (rplacd (input-value (first inputs) lexical-environment)
            (input-value (second inputs) lexical-environment))
    successor))
