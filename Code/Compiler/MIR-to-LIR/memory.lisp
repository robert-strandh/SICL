(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:memref1-instruction) lexical-locations)
  (let ((inputs (cleavir-ir:inputs instruction))
        (outputs (cleavir-ir:outputs instruction)))
    (when (lexical-p (first inputs))
      (insert-memref-before
       instruction
       (first inputs)
       *r11*
       *rax*
       lexical-locations)
      (setf (first inputs) *r11*))
    (when (lexical-p (first outputs))
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
    (when (lexical-p (first inputs))
      (insert-memref-before
       instruction
       (first inputs)
       *r11*
       *rax*
       lexical-locations)
      (setf (first inputs) *r11*))
    (when (lexical-p (second inputs))
      (insert-memref-before
       instruction
       (second inputs)
       *rax*
       *rax*
       lexical-locations)
      (setf (second inputs) *rax*))))

(defmethod process-instruction
    ((instruction cleavir-ir:memref2-instruction) lexical-locations)
  (let ((inputs (cleavir-ir:inputs instruction))
        (outputs (cleavir-ir:outputs instruction)))
    (when (lexical-p (first inputs))
      (insert-memref-before
       instruction
       (first inputs)
       *r11*
       *rax*
       lexical-locations)
      (setf (first inputs) *r11*))
    (when (lexical-p (first outputs))
      (insert-memset-after
       instruction
       *r11*
       (first outputs)
       *rax*
       lexical-locations)
      (setf (first outputs) *r11*))))

(defmethod process-instruction
    ((instruction cleavir-ir:memset2-instruction) lexical-locations)
  (let ((inputs (cleavir-ir:inputs instruction)))
    (when (lexical-p (first inputs))
      (insert-memref-before
       instruction
       (first inputs)
       *r11*
       *rax*
       lexical-locations)
      (setf (first inputs) *r11*))
    (when (lexical-p (third inputs))
      (insert-memref-before
       instruction
       (third inputs)
       *rax*
       *rax*
       lexical-locations)
      (setf (third inputs) *rax*))))
