(cl:in-package #:sicl-hir-to-mir)

(defmethod process-instruction (client
                                (instruction cleavir-ir:car-instruction))
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (raw-pointer-location (raw-pointer-from-tagged-pointer instruction input -1)))
    (change-class instruction 'cleavir-ir:memref1-instruction
                  :address raw-pointer-location)))

(defmethod process-instruction (client
                                (instruction cleavir-ir:cdr-instruction))
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (raw-pointer-location (raw-pointer-from-tagged-pointer instruction input 7)))
    (change-class instruction 'cleavir-ir:memref1-instruction
                  :address raw-pointer-location)))

(defmethod process-instruction (client
                                (instruction cleavir-ir:rplaca-instruction))
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (raw-pointer-location (raw-pointer-from-tagged-pointer instruction input -1))
         (value-location (second (cleavir-ir:inputs instruction))))
    (change-class instruction 'cleavir-ir:memset1-instruction
                  :address raw-pointer-location
                  :value value-location)))

(defmethod process-instruction (client
                                (instruction cleavir-ir:rplacd-instruction))
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (raw-pointer-location (raw-pointer-from-tagged-pointer instruction input 7))
         (value-location (second (cleavir-ir:inputs instruction))))
    (change-class instruction 'cleavir-ir:memset1-instruction
                  :address raw-pointer-location
                  :value value-location)))
