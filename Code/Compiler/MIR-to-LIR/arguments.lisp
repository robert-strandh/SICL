(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:compute-argument-count-instruction)
     lexical-locations)
  (assert (lexical-p (first (cleavir-ir:outputs instruction))))
  (insert-memset-after
   instruction
   *r11*
   (first (cleavir-ir:outputs instruction))
   lexical-locations)
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:memref1-instruction
     :input *rsp*
     :output *r11*)
   instruction)
  (change-class instruction 'cleavir-ir:nop-instruction
                :inputs '()
                :outputs '()))

(defmethod process-instruction
    ((instruction cleavir-ir:argument-instruction)
     lexical-locations)
  (assert (lexical-p (first (cleavir-ir:outputs instruction))))
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
             (cleavir-ir:insert-instruction-before
              (make-instance 'cleavir-ir:unsigned-add-instruction
                :inputs (list *r11*
                              (make-instance 'cleavir-ir:immediate-input
                                :value 2))
                :output *r11*)
              instruction)
             (cleavir-ir:insert-instruction-before
              (make-instance 'cleavir-ir:shift-left-instruction
                :inputs (list *r11*
                              (make-instance 'cleavir-ir:immediate-input
                                :value 2))
                :output *r11*)
              instruction)
             (cleavir-ir:insert-instruction-before
              (make-instance 'cleavir-ir:unsigned-add-instruction
                :inputs (list *r11* *rsp*)
                :output *r11*)
              instruction)
             (cleavir-ir:insert-instruction-before
              (make-instance 'cleavir-ir:memref1-instruction
                :input *r11*
                :output *r11*)
              instruction))
      ;; The value is an immediate input encoding a fixnum, so it is
      ;; multiplied by 2 with respect to the "number" of the argument.
      ;; The topmost stack location contains the argument count, so we
      ;; need to skip that by adding 2 to the value before multiplying
      ;; by 4.
      (let* ((value (cleavir-ir:value (first (cleavir-ir:inputs instruction))))
             (offset (* 4 (+ value 2))))
        (cleavir-ir:insert-instruction-before
         (make-instance 'cleavir-ir:memref2-instruction
           :inputs (list *rsp*
                         (make-instance 'cleavir-ir:immediate-input
                           :value offset))
           :output *r11*)
         instruction)))
  (change-class instruction 'cleavir-ir:nop-instruction
                :inputs '()
                :outputs '()))
