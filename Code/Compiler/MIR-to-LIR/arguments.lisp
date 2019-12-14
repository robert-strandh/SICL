(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:compute-argument-count-instruction)
     lexical-locations)
  (change-class instruction 'cleavir-ir:assignment-instruction
                :input *r9*)
  (unless (typep (first (cleavir-ir:outputs instruction))
                 'cleavir-ir:register-location)
    (insert-memset-after
     instruction
     *r11*
     (first (cleavir-ir:outputs instruction))
     lexical-locations)))

