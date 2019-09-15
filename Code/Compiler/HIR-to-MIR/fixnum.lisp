(cl:in-package #:sicl-hir-to-mir)

(defmethod process-instruction
    (client (instruction cleavir-ir:fixnum-less-instruction))
  (change-class instruction 'cleavir-ir:signed-less-instruction))

(defmethod process-instruction
    (client (instruction cleavir-ir:fixnum-not-greater-instruction))
  (change-class instruction 'cleavir-ir:signed-not-greater-instruction))

(defmethod process-instruction
    (client (instruction cleavir-ir:fixnum-equal-instruction))
  (change-class instruction 'cleavir-ir:eq-instruction))

(defmethod process-instruction
    (client (instruction cleavir-ir:fixnum-add-instruction))
  (change-class instruction 'cleavir-ir:signed-add-instruction))

(defmethod process-instruction
    (client (instruction cleavir-ir:fixnum-sub-instruction))
  (change-class instruction 'cleavir-ir:signed-sub-instruction))
