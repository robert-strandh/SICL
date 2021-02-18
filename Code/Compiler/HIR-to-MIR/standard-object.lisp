(cl:in-package #:sicl-hir-to-mir)

(defmethod  process-instruction
    (client (instruction cleavir-ir:nook-read-instruction) code-object)
  (destructuring-bind (object-location slot-number-location)
      (cleavir-ir:inputs instruction)
    (let* ((rack-location (find-rack instruction object-location))
           (slot-offset-location (compute-slot-offset slot-number-location
                                                      instruction
                                                      3))
           (slot-location (compute-slot-location rack-location
                                                 slot-offset-location
                                                 instruction)))
      (change-class instruction 'cleavir-ir:memref1-instruction
                    :address slot-location))))

(defmethod  process-instruction
    (client (instruction cleavir-ir:nook-write-instruction) code-object)
  (destructuring-bind (object-location slot-number-location value-location)
      (cleavir-ir:inputs instruction)
    (let* ((rack-location (find-rack instruction object-location))
           (slot-offset-location (compute-slot-offset slot-number-location
                                                      instruction
                                                      3))
           (slot-location (compute-slot-location rack-location
                                                 slot-offset-location
                                                 instruction)))
      (change-class instruction 'cleavir-ir:memset1-instruction
                    :address slot-location
                    :value value-location))))
