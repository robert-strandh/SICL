(cl:in-package #:sicl-hir-to-mir)

(defun process-nook-read-instruction
    (instruction object-location slot-number-location)
  (let* ((rack-location (find-rack instruction object-location))
         (slot-offset-location (compute-slot-offset slot-number-location
                                                    instruction
                                                    3))
         (slot-location (compute-slot-location rack-location
                                               slot-offset-location
                                               instruction)))
    (change-class instruction 'cleavir-ir:memref1-instruction
                  :address slot-location)))

(defmethod process-instruction
    (client (instruction cleavir-ir:nook-read-instruction) code-object)
  (destructuring-bind (object-location slot-number-location)
      (cleavir-ir:inputs instruction)
    (process-nook-read-instruction instruction
                                   object-location
                                   slot-number-location)))

(defun process-nook-write-instruction
    (instruction object-location slot-number-location value-location)
  (let* ((rack-location (find-rack instruction object-location))
         (slot-offset-location (compute-slot-offset slot-number-location
                                                    instruction
                                                    3))
         (slot-location (compute-slot-location rack-location
                                               slot-offset-location
                                               instruction)))
    (change-class instruction 'cleavir-ir:memset1-instruction
                  :address slot-location
                  :value value-location
                  :outputs '())))

(defmethod process-instruction
    (client (instruction cleavir-ir:nook-write-instruction) code-object)
  (destructuring-bind (object-location slot-number-location value-location)
      (cleavir-ir:inputs instruction)
    (process-nook-write-instruction instruction
                                    object-location
                                    slot-number-location
                                    value-location)))

(defmethod process-instruction
    (client
     (instruction cleavir-ir:standard-object-class-of-instruction)
     code-object)
  (destructuring-bind (object-location)
      (cleavir-ir:inputs instruction)
    (change-class instruction
                  'cleavir-ir:memref2-instruction
                  :base-address object-location
                  :offset (make-instance 'cleavir-ir:immediate-input
                            :value -5))))
