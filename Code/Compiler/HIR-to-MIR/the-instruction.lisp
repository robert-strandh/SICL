(cl:in-package #:sicl-hir-to-mir)

(defmethod process-instruction
    (client (instruction cleavir-ir:the-instruction) code-object)
  (change-class instruction 'cleavir-ir:nop-instruction
                :outputs '()))
