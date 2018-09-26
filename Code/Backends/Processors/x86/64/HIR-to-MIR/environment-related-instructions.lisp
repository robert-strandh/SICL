(cl:in-package #:sicl-hir-to-mir)

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:read-cell-instruction))
  (change-class instruction 'cleavir-ir:memref2-instruction
                :offset -1))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:write-cell-instruction))
  (change-class instruction 'cleavir-ir:memset2-instruction
                :offset -1))
