(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:bind-instruction) lexical-locations)
  (process-funcall instruction lexical-locations))
