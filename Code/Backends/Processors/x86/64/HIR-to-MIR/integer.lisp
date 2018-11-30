(cl:in-package #:sicl-hir-to-mir)

(defmethod translate ((client sicl-client:sicl-x86-64)
                      (instruction cleavir-ir:fixnum-add-instruction))
  (make-instance 'cleavir-ir:signed-add-instruction
    :inputs (cleavir-ir:inputs instruction)
    :outputs (cleavir-ir:outputs instruction)
    :successors (cleavir-ir:successors instruction)))

(defmethod translate ((client sicl-client:sicl-x86-64)
                      (instruction cleavir-ir:fixnum-sub-instruction))
  (make-instance 'cleavir-ir:signed-sub-instruction
    :inputs (cleavir-ir:inputs instruction)
    :outputs (cleavir-ir:outputs instruction)
    :successors (cleavir-ir:successors instruction)))

(defmethod translate ((client sicl-client:sicl-x86-64)
                      (instruction cleavir-ir:fixnum-less-instruction))
  (make-instance 'cleavir-ir:signed-less-instruction
    :inputs (cleavir-ir:inputs instruction)
    :outputs (cleavir-ir:outputs instruction)
    :successors (cleavir-ir:successors instruction)))

(defmethod translate ((client sicl-client:sicl-x86-64)
                      (instruction cleavir-ir:fixnum-not-greater-instruction))
  (make-instance 'cleavir-ir:signed-not-greater-instruction
    :inputs (cleavir-ir:inputs instruction)
    :outputs (cleavir-ir:outputs instruction)
    :successors (cleavir-ir:successors instruction)))

(defmethod translate ((client sicl-client:sicl-x86-64)
                      (instruction cleavir-ir:fixnum-equal-instruction))
  (make-instance 'cleavir-ir:equal-instruction
    :inputs (cleavir-ir:inputs instruction)
    :outputs (cleavir-ir:outputs instruction)
    :successors (cleavir-ir:successors instruction)))
