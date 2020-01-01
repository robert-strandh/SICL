(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:memref1-instruction) lexical-locations)
  (let ((inputs (cleavir-ir:inputs instruction))
        (outputs (cleavir-ir:outputs instruction)))
    (when (typep (first inputs) 'cleavir-ir:lexical-location)
      (insert-memref-before
       instruction
       (first inputs)
       *r11*
       *rax*
       lexical-locations)
      (setf (first inputs) *r11*))
    (when (typep (first outputs) 'cleavir-ir:lexical-location)
      (insert-memset-after
       instruction
       *r11*
       (first outputs)
       *rax*
       lexical-locations)
      (setf (first outputs) *r11*))))

(defmethod process-instruction
    ((instruction cleavir-ir:memset1-instruction) lexical-locations)
  (let ((inputs (cleavir-ir:inputs instruction)))
    (when (typep (first inputs) 'cleavir-ir:lexical-location)
      (insert-memref-before
       instruction
       (first inputs)
       *r11*
       *rax*
       lexical-locations)
      (setf (first inputs) *r11*))
    (when (typep (second inputs) 'cleavir-ir:lexical-location)
      (insert-memref-before
       instruction
       (second inputs)
       *rax*
       *rax*
       lexical-locations)
      (setf (second inputs) *rax*))))
