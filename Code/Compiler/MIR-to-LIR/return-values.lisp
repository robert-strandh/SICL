(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:initialize-values-instruction)
     lexical-locations)
  (change-class instruction 'cleavir-ir:assignment-instruction
                :output *rdi*))

