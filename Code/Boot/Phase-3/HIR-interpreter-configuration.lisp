(cl:in-package #:sicl-boot-phase-3)

(defmethod sicl-hir-interpreter:interpret-instruction
    ((client sicl-boot:client)
     (instruction cleavir-ir:nook-read-instruction)
     lexical-environment)
  (destructuring-bind (object-input index-input)
      (cleavir-ir:inputs instruction)
    (let* ((standard-object-input-value
             (sicl-hir-interpreter:input-value object-input lexical-environment))
           (index-input-value
             (sicl-hir-interpreter:input-value index-input lexical-environment))
           (output (first (cleavir-ir:outputs instruction))))
      (setf (sicl-hir-interpreter:lexical-value output lexical-environment)
            (if (zerop index-input-value)
                ;; Then the stamp is asked for
                (flet ((unique-number (class-name)
                         (funcall (sicl-genv:fdefinition
                                   'sicl-clos::unique-number sicl-boot:*e3*)
                                  (sicl-genv:find-class class-name sicl-boot:*e3*))))
                  (cond ((null standard-object-input-value)
                         (unique-number 'null))
                        ((symbolp standard-object-input-value)
                         (unique-number 'symbol))
                        ((typep standard-object-input-value 'sicl-boot::header)
                         (let ((rack (slot-value standard-object-input-value 'sicl-boot::%rack)))
                           (aref rack 0)))
                        (t (error "Can't deal with object ~s"
                                  standard-object-input-value))))
                (let ((rack (slot-value standard-object-input-value 'sicl-boot::%rack)))
                  (aref rack index-input-value))))))
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
