(cl:in-package #:sicl-hir-to-mir)

(defun find-slot (object-location slot-number slot-address instruction)
  (let ((rack-location (make-instance 'cleavir-ir:lexical-location
                        :name '#:rack-location)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:memref2-instruction
       :offset 3
       :inputs (list object-location)
       :outputs (list rack-location)
       :successors (list instruction))
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:unsigned-add-instruction
       :inputs (list rack-location slot-number)
       :outputs (list slot-location)
       :successors (list instruction))
     instruction)))

(defmethod  process-instruction (client
                                 (instruction cleavir-ir:slot-read-instruction))
  (let ((slot-location (make-instance 'cleavir-ir:lexical-location
                        :name '#:slot-location)))
    (destructuring-bind (object-location slot-number)
        (cleavir-ir:inputs instruction)
      (find-slot object-location slot-number slot-location instruction)
      (change-class instruction 'cleavir-ir:memref2-instruction
                    :offset -7
                    :inputs (list slot-location)
                    :outputs (cleavir-ir:outputs instruction)
                    :successors (cleavir-ir:successors instruction)))))

(defmethod  process-instruction (client
                                 (instruction cleavir-ir:slot-write-instruction))
  (let ((slot-location (make-instance 'cleavir-ir:lexical-location
                        :name '#:slot-location)))
    (destructuring-bind (object-location slot-number value-location)
        (cleavir-ir:inputs instruction)
      (find-slot object-location slot-number slot-location instruction)
      (change-class instruction 'cleavir-ir:memset2-instruction
                    :offset -7
                    :inputs (list slot-location value-location)
                    :successors (cleavir-ir:successors instruction)))))
