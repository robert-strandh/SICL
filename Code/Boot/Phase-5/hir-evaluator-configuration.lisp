(cl:in-package sicl-boot-phase-5)

(defmethod sicl-hir-evaluator:instruction-thunk
    ((client client)
     (instruction cleavir-ir:standard-object-class-of-instruction)
     lexical-environment)
  (sicl-hir-evaluator:make-thunk
      (client instruction lexical-environment :inputs 1 :outputs 1)
    (setf (sicl-hir-evaluator:output 0)
          (slot-value (sicl-hir-evaluator:input 0)
                      'sicl-boot::%class))
    (sicl-hir-evaluator:successor 0)))
