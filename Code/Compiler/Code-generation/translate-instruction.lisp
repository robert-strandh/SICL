(cl:in-package #:sicl-code-generation)

(defgeneric translate-instruction (instruction))

(defmethod translate-instruction
    ((instruction 'cleavir-ir:nop-instruction))
  '())

(defmethod translate-instruction
    ((instruction 'cleavir-ir:enter-instruction))
  '())

(defmethod translate-instruction
    ((instruction cleavir-ir:assignment-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "MOV"
    :operands
    (list
     (translate-datum (first (cleavir-ir:outputs instruction)))
     (translate-datum (first (cleavir-ir:inputs instruction))))))

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
