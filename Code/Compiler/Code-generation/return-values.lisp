(cl:in-package #:sicl-code-generation)

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:return-value-instruction))
  '())
