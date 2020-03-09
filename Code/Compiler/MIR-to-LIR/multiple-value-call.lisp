(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction) lexical-locations)
  (process-funcall instruction lexical-locations))
