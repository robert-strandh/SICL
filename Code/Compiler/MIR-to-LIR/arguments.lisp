(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:compute-argument-count-instruction)
     lexical-locations)
  (change-class instruction 'cleavir-ir:assignment-instruction
                :input *r9*)
  (unless (typep (first (cleavir-ir:outputs instruction))
                 'cleavir-ir:register-location)
    (insert-memset-after
     instruction
     *r9*
     (first (cleavir-ir:outputs instruction))
     *rax*
     lexical-locations)))

(defmethod process-instruction
    ((instruction cleavir-ir:argument-instruction)
     lexical-locations)
  (let ((shift-count-input
          (make-instance 'cleavir-ir:immediate-input :value 2)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:assignment-instruction
       :input (first (cleavir-ir:inputs instruction))
       :output *rax*)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:shift-left-instruction
       :inputs (list *rax* shift-count-input)
       :output *rax*)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:assignment-instruction
       :input *rsp*
       :output *r11*)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:unsigned-add-instruction
       :inputs (list *r11* *rax*)
       :output *r11*)
     instruction)
    (change-class instruction 'cleavir-ir:memref1-instruction
                  :address *r11*)))
