(cl:in-package #:sicl-code-generation)

(defgeneric translate-instruction (instruction))

(defmethod translate-instruction
    ((instruction cleavir-ir:assignment-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "MOV"
    :operands (list (translate-datum (first (cleavir-ir:outputs instruction)))
                    (translate-datum (first (cleavir-ir:inputs instruction))))))
