(cl:in-package #:sicl-hir-to-mir)

(defgeneric hir-to-mir (client instruction))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64) instruction)
  nil)

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:enter-instruction))
  nil)

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:eq-instruction))
  (change-class instruction 'cleavir-ir:equal-instruction))
