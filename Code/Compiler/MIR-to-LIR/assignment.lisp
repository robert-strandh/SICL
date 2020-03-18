(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:assignment-instruction) lexical-locations)
  (insert-memset-after
   instruction
   *r11*
   (first (cleavir-ir:outputs instruction))
   lexical-locations)
  (if (lexical-p (first (cleavir-ir:inputs instruction)))
      (progn (insert-memref-before
              instruction
              (first (cleavir-ir:inputs instruction))
              *r11*
              lexical-locations)
             (change-class instruction 'cleavir-ir:nop-instruction
                           :inputs '()
                           :outputs '()))
      (setf (cleavir-ir:outputs instruction) (list *r11*))))
