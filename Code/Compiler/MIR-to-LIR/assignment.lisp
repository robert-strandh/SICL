(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:assignment-instruction) lexical-locations)
  (call-next-method)
  (change-class instruction 'nop-instruction
                :inputs '()
                :outputs '()))
