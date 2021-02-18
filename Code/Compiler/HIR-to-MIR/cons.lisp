(cl:in-package #:sicl-hir-to-mir)

(defmethod process-instruction (client
                                (instruction cleavir-ir:car-instruction)
                                code-object)
  (change-class instruction 'cleavir-ir:memref2-instruction
                :inputs (list (first (cleavir-ir:inputs instruction))
                              (make-instance 'cleavir-ir:immediate-input
                                :value -1))))

(defmethod process-instruction (client
                                (instruction cleavir-ir:cdr-instruction)
                                code-object)
  (change-class instruction 'cleavir-ir:memref2-instruction
                :inputs (list (first (cleavir-ir:inputs instruction))
                              (make-instance 'cleavir-ir:immediate-input
                                :value 7))))

(defmethod process-instruction (client
                                (instruction cleavir-ir:rplaca-instruction)
                                code-object)
  (destructuring-bind (cons-input value-input)
      (cleavir-ir:inputs instruction)
    (change-class instruction 'cleavir-ir:memset2-instruction
                  :inputs (list cons-input
                                (make-instance 'cleavir-ir:immediate-input
                                  :value -1)
                                value-input))))

(defmethod process-instruction (client
                                (instruction cleavir-ir:rplacd-instruction)
                                code-object)
  (destructuring-bind (cons-input value-input)
      (cleavir-ir:inputs instruction)
    (change-class instruction 'cleavir-ir:memset2-instruction
                  :inputs (list cons-input
                                (make-instance 'cleavir-ir:immediate-input
                                  :value 7)
                                value-input))))
