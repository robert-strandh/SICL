(cl:in-package #:sicl-hir-to-mir)

(defun find-slot (object-location slot-number slot-address successors)
  (let ((rack-address (make-instance 'cleavir-ir:lexical-location
                        :name '#:rack-address)))
    (make-instance 'cleavir-ir:memref2-instruction
      :offset 3
      :inputs (list object-location)
      :outputs (list rack-address)
      :successors
      (list (make-instance 'cleavir-ir:unsigned-add-instruction
              :inputs (list rack-address slot-number)
              :outputs (list slot-address)
              :successors successors)))))

(defgeneric process-slot-read-instruction (client instruction))

(defmethod  process-slot-read-instruction (client instruction)
  (let ((slot-address (make-instance 'cleavir-ir:lexical-location
                        :name '#:slot-address)))
    (destructuring-bind (object-location slot-number)
        (cleavir-ir:inputs instruction)
      (find-slot
       object-location slot-number slot-address
       (list (make-instance 'cleavir-ir:memref2-instruction
               :offset -7
               :inputs (list slot-address)
               :outputs (cleavir-ir:outputs instruction)
               :successors (cleavir-ir:successors instruction)))))))

(defun process-slot-read-instructions (client initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'cleavir-ir:slot-read-instruction)
       (process-slot-read-instruction client instruction )))
   initial-instruction))

(defgeneric process-slot-write-instruction (client instruction))

(defmethod  process-slot-write-instruction (client instruction)
  (let ((slot-address (make-instance 'cleavir-ir:lexical-location
                        :name '#:slot-address)))
    (destructuring-bind (object-location slot-number value-location)
        (cleavir-ir:inputs instruction)
      (find-slot
       object-location slot-number slot-address
       (list (make-instance 'cleavir-ir:memset2-instruction
               :offset -7
               :inputs (list slot-address value-location)
               :successors (cleavir-ir:successors instruction)))))))

(defun process-slot-write-instructions (client initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'cleavir-ir:slot-write-instruction)
       (process-slot-write-instruction client instruction )))
   initial-instruction))
