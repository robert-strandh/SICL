(cl:in-package #:sicl-hir-to-mir)

(defmethod  process-instruction
    (client (instruction cleavir-ir:nook-read-instruction))
  (destructuring-bind (object-location slot-number-location)
      (cleavir-ir:inputs instruction)
    (let* ((rack-location (find-rack instruction object-location))
           (first-slot-location (skip-rack-prefix instruction rack-location 2))
           (slot-offset-location (compute-slot-offset slot-number-location
                                                      instruction
                                                      3))
           (slot-location (compute-slot-location first-slot-location
                                                 slot-offset-location
                                                 instruction)))
      (change-class instruction 'cleavir-ir:memref1-instruction
                    :inputs (list slot-location)
                    :outputs (cleavir-ir:outputs instruction)))))

(defmethod  process-instruction
    (client (instruction cleavir-ir:nook-write-instruction))
  (destructuring-bind (object-location slot-number-location value-location)
      (cleavir-ir:inputs instruction)
    (let* ((rack-location (find-rack instruction object-location))
           (first-slot-location (skip-rack-prefix instruction rack-location 2))
           (slot-offset-location (compute-slot-offset slot-number-location
                                                      instruction
                                                      3))
           (slot-location (compute-slot-location first-slot-location
                                                 slot-offset-location
                                                 instruction)))
      (change-class instruction 'cleavir-ir:memset1-instruction
                    :inputs (list slot-location value-location)))))
