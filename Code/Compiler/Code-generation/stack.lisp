(cl:in-package #:sicl-code-generation)

(defmethod translate-simple-instruction
    ((instruction sicl-ir:push-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "PUSH"
    :operands
    (list (translate-input (first (cleavir-ir:inputs instruction))
                           instruction))))

(defmethod translate-simple-instruction
    ((instruction sicl-ir:pop-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "POP"
    :operands
    (list (translate-output (first (cleavir-ir:outputs instruction))
                            instruction))))
