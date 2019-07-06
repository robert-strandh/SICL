(cl:in-package #:sicl-hir-to-mir)

(defmethod process-instruction (client
                                (instruction cleavir-ir:read-cell-instruction))
  (change-class instruction 'cleavir-ir:memref2-instruction
                :offset -1
                :inputs (cleavir-ir:inputs instruction)
                :outputs (cleavir-ir:outputs instruction)
                :successors (cleavir-ir:successors instruction)))

(defmethod process-instruction (client
                                (instruction cleavir-ir:write-cell-instruction))
  (change-class instruction 'cleavir-ir:memset2-instruction
                :offset -1
                :inputs (cleavir-ir:inputs instruction)
                :outputs (cleavir-ir:outputs instruction)
                :successors (cleavir-ir:successors instruction)))
