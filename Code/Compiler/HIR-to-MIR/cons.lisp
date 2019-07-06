(cl:in-package #:sicl-hir-to-mir)

(defgeneric process-car-instruction (client instruction))

(defmethod process-car-instruction (client instruction)
  (change-class instruction 'cleavir-ir:memref2-instruction
                :offset -1
                :inputs (cleavir-ir:inputs instruction)
                :outputs (cleavir-ir:outputs instruction)
                :successors (cleavir-ir:successors instruction)))
