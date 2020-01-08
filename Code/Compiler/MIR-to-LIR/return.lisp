(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:return-instruction)
     lexical-locations)
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:assignment-instruction
     :input *rsp*
     :output *r11*)
   instruction)
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:assignment-instruction
     :input *rbp*
     :output *r8*)
   instruction)
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:unsigned-sub-instruction
     :inputs (list *r8* (make-instance 'cleavir-ir:immediate-input :value 8))
     :output *r8*)
   instruction)
  (loop repeat 15
        do (cleavir-ir:insert-instruction-before
            (make-instance 'cleavir-ir:unsigned-sub-instruction
              :inputs (list *r11* (make-instance 'cleavir-ir:immediate-input :value 8))
              :output *r11*)
            instruction)
           (cleavir-ir:insert-instruction-before
            (make-instance 'cleavir-ir:unsigned-sub-instruction
              :inputs (list *r8* (make-instance 'cleavir-ir:immediate-input :value 8))
              :output *r8*)
            instruction)
           (cleavir-ir:insert-instruction-before
            (make-instance 'cleavir-ir:memref1-instruction
              :address *r11*
              :output *rbx*)
            instruction)
           (cleavir-ir:insert-instruction-before
            (make-instance 'cleavir-ir:memset1-instruction
              :address *r8*
              :value *rbx*)
            instruction))
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:assignment-instruction
     :input *rbp*
     :output *rsp*)
   instruction)
  (make-instance 'cleavir-ir:unsigned-sub-instruction
    :inputs (list *rsp* (make-instance 'cleavir-ir:immediate-input :value 8))
    :output *rsp*)
  instruction)
