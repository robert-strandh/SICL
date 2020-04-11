(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:fixnump-instruction) lexical-locations)
  (insert-memref-before
   instruction
   (first (cleavir-ir:inputs instruction))
   *r11*
   lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:consp-instruction) lexical-locations)
  (insert-memref-before
   instruction
   (first (cleavir-ir:inputs instruction))
   *r11*
   lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:characterp-instruction) lexical-locations)
  (insert-memref-before
   instruction
   (first (cleavir-ir:inputs instruction))
   *r11*
   lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:single-float-p-instruction) lexical-locations)
  (insert-memref-before
   instruction
   (first (cleavir-ir:inputs instruction))
   *r11*
   lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:standard-object-p-instruction) lexical-locations)
  (insert-memref-before
   instruction
   (first (cleavir-ir:inputs instruction))
   *r11*
   lexical-locations))
