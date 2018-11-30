(cl:in-package #:sicl-hir-to-mir)

(defmethod translate ((client sicl-client:sicl-x86-64)
                      (instruction cleavir-ir:read-cell-instruction))
  (make-instance 'cleavir-ir:memref2-instruction
    :offset -1
    :inputs (cleavir-ir:inputs instruction)
    :outputs (cleavir-ir:outputs instruction)
    :successors (cleavir-ir:successors instruction)))

(defmethod translate ((client sicl-client:sicl-x86-64)
                      (instruction cleavir-ir:write-cell-instruction))
  (make-instance 'cleavir-ir:memset2-instruction
    :offset -1
    :inputs (cleavir-ir:inputs instruction)
    :outputs (cleavir-ir:outputs instruction)
    :successors (cleavir-ir:successors instruction)))
