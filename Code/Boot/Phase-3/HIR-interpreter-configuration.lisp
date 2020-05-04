(cl:in-package #:sicl-boot-phase-3)

(defmethod sicl-hir-interpreter:interpret-instruction
    ((client sicl-boot:client)
     (instruction cleavir-ir:nook-read-instruction)
     lexical-environment)
  (destructuring-bind (object-input index-input)
      (cleavir-ir:inputs instruction)
    (let ((object
            (sicl-hir-interpreter:input-value object-input lexical-environment))
          (index
            (sicl-hir-interpreter:input-value index-input lexical-environment))
          (output (first (cleavir-ir:outputs instruction))))
      (setf (sicl-hir-interpreter:lexical-value output lexical-environment)
            (aref (slot-value object 'sicl-boot::%rack) index))))
  (first (cleavir-ir:successors instruction)))


(defmethod sicl-hir-interpreter:interpret-instruction
    ((client sicl-boot:client)
     (instruction cleavir-ir:nook-write-instruction)
     lexical-environment)
  (destructuring-bind (object-input index-input value-input)
      (cleavir-ir:inputs instruction)
    (let ((object
            (sicl-hir-interpreter:input-value object-input lexical-environment))
          (index
            (sicl-hir-interpreter:input-value index-input lexical-environment))
          (value
            (sicl-hir-interpreter:input-value value-input lexical-environment)))
      (setf (aref (slot-value object 'sicl-boot::%rack) index) value)))
  (first (cleavir-ir:successors instruction)))
