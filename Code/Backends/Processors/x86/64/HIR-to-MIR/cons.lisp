(cl:in-package #:sicl-hir-to-mir)

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:car-instruction))
  (change-class instruction 'cleavir-ir:memref2-instruction
                :offset -1))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:cdr-instruction))
  (change-class instruction 'cleavir-ir:memref2-instruction
                :offset 7))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:rplaca-instruction))
  (change-class instruction 'cleavir-ir:memset2-instruction
                :offset -1))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:rplacd-instruction))
  (change-class instruction 'cleavir-ir:memset2-instruction
                :offset 7))
