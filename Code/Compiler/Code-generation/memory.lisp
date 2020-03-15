(cl:in-package #:sicl-code-generation)

(defmethod translate-instruction
    ((instruction cleavir-ir:memref1-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "MOV"
    :operands
    (list
     (translate-datum (first (cleavir-ir:outputs instruction)))
     (make-instance 'cluster:memory-operand
       :base-register (translate-datum (first (cleavir-ir:inputs instruction)))
       :size 64))))

(defmethod translate-instruction
    ((instruction cleavir-ir:memset1-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "MOV"
    :operands
    (list
     (make-instance 'cluster:memory-operand
       :base-register (translate-datum (first (cleavir-ir:inputs instruction)))
       :size 64)
     (translate-datum (second (cleavir-ir:inputs instruction))))))
