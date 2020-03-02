(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:unwind-instruction) lexical-locations)
  (process-funcall instruction lexical-locations))
