(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:compute-return-value-count-instruction)
     lexical-environment)
  (setf (lexical-value
         (first (cleavir-ir:outputs instruction)) lexical-environment)
        (length *global-values-location*))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:return-value-instruction)
     lexical-environment)
  (let ((input-value
          (input-value
           (first (cleavir-ir:inputs instruction)) lexical-environment)))
    (assert (or (zerop input-value)
                (< input-value (length *global-values-location*))))
    (setf (lexical-value
           (first (cleavir-ir:outputs instruction)) lexical-environment)
          (nth input-value *global-values-location*)))
  (first (cleavir-ir:successors instruction)))
