(cl:in-package #:sicl-hir-to-mir)

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:fixnum-add-instruction))
  (change-class instruction 'cleavir-ir:signed-add-instruction))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:fixnum-sub-instruction))
  (change-class instruction 'cleavir-ir:signed-sub-instruction))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:fixnum-less-instruction))
  (change-class instruction 'cleavir-ir:signed-less-instruction))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:fixnum-not-greater-instruction))
  (change-class instruction 'cleavir-ir:signed-not-greater-instruction))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:fixnum-equal-instruction))
  (change-class instruction 'cleavir-ir:equal-instruction))
