(cl:in-package #:sicl-hir-to-mir)

(defmethod process-instruction (client
                                (instruction cleavir-ir:car-instruction))
  (let ((offset-input (make-instance 'cleavir-ir:immediate-input :value 1))
        (raw-pointer-location (make-instance 'cleavir-ir:raw-integer :size 64)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:unsigned-sub-instruction
       :minuend (first (cleavir-ir:inputs instruction))
       :subtrahend offset-input
       :output raw-pointer-location
       :successor instruction)
     instruction)
    (change-class instruction 'cleavir-ir:memref1-instruction
                  :input raw-pointer-location
                  :outputs (cleavir-ir:outputs instruction)
                  :successors (cleavir-ir:successors instruction))))

(defmethod process-instruction (client
                                (instruction cleavir-ir:cdr-instruction))
  (change-class instruction 'cleavir-ir:memref2-instruction
                :offset 7
                :inputs (cleavir-ir:inputs instruction)
                :outputs (cleavir-ir:outputs instruction)
                :successors (cleavir-ir:successors instruction)))

(defmethod process-instruction (client
                                (instruction cleavir-ir:rplaca-instruction))
  (change-class instruction 'cleavir-ir:memset2-instruction
                :offset -1
                :inputs (cleavir-ir:inputs instruction)
                :outputs (cleavir-ir:outputs instruction)
                :successors (cleavir-ir:successors instruction)))

(defmethod process-instruction (client
                                (instruction cleavir-ir:rplacd-instruction))
  (change-class instruction 'cleavir-ir:memset2-instruction
                :offset 7
                :inputs (cleavir-ir:inputs instruction)
                :outputs (cleavir-ir:outputs instruction)
                :successors (cleavir-ir:successors instruction)))
