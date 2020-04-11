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
