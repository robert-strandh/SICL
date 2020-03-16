(cl:in-package #:sicl-code-generation)

(defmethod translate-instruction
    ((instruction sicl-ir:push-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "PUSH"
    :operands
    (list (translate-datum (first (cleavir-ir:inputs instruction))))))

(defmethod translate-instruction
    ((instruction sicl-ir:push-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "POP"
    :operands
    (list (translate-datum (first (cleavir-ir:outputs instruction))))))
