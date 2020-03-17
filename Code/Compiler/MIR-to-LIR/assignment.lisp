(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:assignment-instruction) lexical-locations)
  (insert-memref-before
   instruction
   (first (cleavir-ir:inputs instruction))
   *r11*
   lexical-locations)
  (insert-memset-after
   instruction
   *r11*
   (first (cleavir-ir:outputs instruction))
   lexical-locations))
