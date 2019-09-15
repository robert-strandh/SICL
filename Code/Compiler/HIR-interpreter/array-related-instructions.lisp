(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:aref-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (array-input-value (input-value (first inputs)
                                         lexical-environment))
         (index-input-value (input-value (second inputs)
                                         lexical-environment))
         (output (first (cleavir-ir:outputs instruction)))
         (successor (first (cleavir-ir:successors instruction))))
    (setf (lexical-value output lexical-environment)
          (row-major-aref array-input-value index-input-value))
    successor))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:aset-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (array-input-value (input-value (first inputs)
                                         lexical-environment))
         (index-input-value (input-value (second inputs)
                                         lexical-environment))
         (object-input-value (input-value  (third inputs)
                                           lexical-environment))
         (successor (first (cleavir-ir:successors instruction))))
    (setf (row-major-aref array-input-value index-input-value) object-input-value)
    successor))
