(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:assignment-instruction) lexical-locations)
  (when (lexical-p (first (cleavir-ir:inputs instruction)))
    (insert-memref-before
     instruction
     (first (cleavir-ir:inputs instruction))
     *r11*
     lexical-locations)
    (setf (cleavir-ir:inputs instruction) (list *r11*)))
  (insert-memset-after
   instruction
   *r11*
   (first (cleavir-ir:outputs instruction))
   lexical-locations))
