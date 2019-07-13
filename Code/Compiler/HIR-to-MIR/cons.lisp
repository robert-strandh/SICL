(cl:in-package #:sicl-hir-to-mir)

(defmethod process-instruction (client
                                (instruction cleavir-ir:car-instruction))
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (raw-pointer-location (raw-pointer-from-tagged-pointer instruction input -1)))
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
