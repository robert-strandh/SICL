(cl:in-package #:sicl-hir-to-mir)

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:slot-read-instruction))
  (let* ((inputs (cleavir-ir:inputs instruction))
         (object-location (first inputs))
         (slot-number (second inputs))
         (outputs (cleavir-ir:outputs instruction))
         (result-location (first outputs))
         (successors (cleavir-ir:successors instruction))
         (successor (first successors))
         (origin (cleavir-ir:origin instruction))
         (policy (cleavir-ir:policy instruction))
         (d2 (make-instance 'cleavir-ir:lexical-location
               :name '#:slot-address))
         (d1 (make-instance 'cleavir-ir:lexical-location
               :name '#:rack-address))
         (i3 (make-instance 'cleavir-ir:memref2-instruction
               :origin origin
               :policy policy
               :offset -7
               :inputs (list d2)
               :outputs (list result-location)))
         (i2 (make-instance 'cleavir-ir:unsigned-add-instruction
               :origin origin
               :policy policy
               :inputs (list d1 slot-number)
               :outputs (list d2)
               :successors (list i3)))
         (i1 (make-instance 'cleavir-ir:memref2-instruction
               :origin origin
               :policy policy
               :offset 3
               :inputs (list object-location)
               :outputs (list d1)
               :successors (list i2))))
    (setf (cleavir-ir:predecessors i3) (list i2))
    (setf (cleavir-ir:predecessors i2) (list i1))
    (setf (cleavir-ir:predecessors i1) (list instruction))
    (cleavir-ir:insert-instruction-before i3 successor)
    (setf (cleavir-ir:predecessors successor)
          (remove instruction (cleavir-ir:predecessors successor)))
    (change-class instruction 'cleavir-ir:nop-instruction
                  :successors (list i1))))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64)
                       (instruction cleavir-ir:slot-write-instruction))
  (let* ((inputs (cleavir-ir:inputs instruction))
         (object-location (first inputs))
         (slot-number (second inputs))
         (value-location (third inputs))
         (outputs (cleavir-ir:outputs instruction))
         (result-location (first outputs))
         (successors (cleavir-ir:successors instruction))
         (successor (first successors))
         (origin (cleavir-ir:origin instruction))
         (policy (cleavir-ir:policy instruction))
         (d2 (make-instance 'cleavir-ir:lexical-location
               :name '#:slot-address))
         (d1 (make-instance 'cleavir-ir:lexical-location
               :name '#:rack-address))
         (i3 (make-instance 'cleavir-ir:memset2-instruction
               :origin origin
               :policy policy
               :offset -7
               :inputs (list d2 value-location)
               :outputs (list result-location)))
         (i2 (make-instance 'cleavir-ir:unsigned-add-instruction
               :origin origin
               :policy policy
               :inputs (list d1 slot-number)
               :outputs (list d2)
               :successors (list i3)))
         (i1 (make-instance 'cleavir-ir:memset2-instruction
               :origin origin
               :policy policy
               :offset 3
               :inputs (list object-location)
               :outputs (list d1)
               :successors (list i2))))
    (setf (cleavir-ir:predecessors i3) (list i2))
    (setf (cleavir-ir:predecessors i2) (list i1))
    (setf (cleavir-ir:predecessors i1) (list instruction))
    (cleavir-ir:insert-instruction-before i3 successor)
    (setf (cleavir-ir:predecessors successor)
          (remove instruction (cleavir-ir:predecessors successor)))
    (change-class instruction 'cleavir-ir:nop-instruction
                  :successors (list i1))))
