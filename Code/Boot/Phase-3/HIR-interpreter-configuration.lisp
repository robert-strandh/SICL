(cl:in-package #:sicl-boot-phase-3)

(defmethod sicl-hir-evaluator:instruction-thunk
    ((client sicl-boot:client)
     (instruction cleavir-ir:nook-read-instruction)
     lexical-environment)
  (sicl-hir-evaluator:make-thunk (client instruction lexical-environment :inputs 2 :outputs 1)
    (setf (sicl-hir-evaluator:output 0)
          (if (zerop (sicl-hir-evaluator:input 1))
              ;; The stamp is asked for
              (flet ((unique-number (class-name)
                       (funcall (sicl-genv:fdefinition
                                 'sicl-clos::unique-number sicl-boot:*e3*)
                                (sicl-genv:find-class class-name sicl-boot:*e3*))))
                (typecase (sicl-hir-evaluator:input 0)
                  (null (unique-number 'null))
                  (symbol (unique-number 'symbol))
                  (sicl-boot::header
                   (let ((rack (slot-value (sicl-hir-evaluator:input 0) 'sicl-boot::%rack)))
                     (aref rack 0)))
                  (t (error "Can't compute the stamp of ~s"
                            (sicl-hir-evaluator:input 0)))))
              (let ((rack (slot-value (sicl-hir-evaluator:input 0) 'sicl-boot::%rack)))
                (aref rack (sicl-hir-evaluator:input 1)))))
    (sicl-hir-evaluator:successor 0)))

(defmethod sicl-hir-evaluator:instruction-thunk
    ((client sicl-boot:client)
     (instruction cleavir-ir:nook-write-instruction)
     lexical-environment)
  (sicl-hir-evaluator:make-thunk (client instruction lexical-environment :inputs 3)
    (setf (aref (slot-value (sicl-hir-evaluator:input 0) 'sicl-boot::%rack)
                (sicl-hir-evaluator:input 1))
          (sicl-hir-evaluator:input 2))
    (sicl-hir-evaluator:successor 0)))
