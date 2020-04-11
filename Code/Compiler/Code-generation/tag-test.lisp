(cl:in-package #:sicl-code-generation)

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixnump-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "BT"
    :operands
    (list (translate-datum (first (cleavir-ir:inputs instruction)))
          (make-instance 'cluster:immediate-operand :value 0))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnump-instruction) next)
  (cons
   (translate-simple-instruction instruction)
   (compute-branches instruction next "JC" "JNC")))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:consp-instruction))
  (list
   (make-instance 'cluster:code-command
     :mnemonic "AND"
     :operands
     (list (translate-datum (first (cleavir-ir:inputs instruction)))
           (make-instance 'cluster:immediate-operand :value #b111)))
   (make-instance 'cluster:code-command
     :mnemonic "CMP"
     :operands
     (list (translate-datum (first (cleavir-ir:inputs instruction)))
           (make-instance 'cluster:immediate-operand :value #b001)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:consp-instruction) next)
  (append
   (translate-simple-instruction instruction)
   (compute-branches instruction next "JNE" "JE")))
