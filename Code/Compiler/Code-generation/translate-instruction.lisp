(cl:in-package #:sicl-code-generation)

(defgeneric translate-instruction (instruction))

(defmethod translate-instruction
    ((instruction cleavir-ir:nop-instruction))
  '())

(defmethod translate-instruction
    ((instruction cleavir-ir:unreachable-instruction))
  '())

(defmethod translate-instruction
    ((instruction cleavir-ir:enter-instruction))
  '())

(defmethod translate-instruction
    ((instruction sicl-ir:breakpoint-instruction))
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
    ((instruction cleavir-ir:shift-left-instruction))
  (assert (eq (first (cleavir-ir:inputs instruction))
              (first (cleavir-ir:outputs instruction))))
  (make-instance 'cluster:code-command
    :mnemonic "SHL"
    :operands
    (list
     (translate-datum (first (cleavir-ir:outputs instruction)))
     (translate-datum (second (cleavir-ir:inputs instruction))))))

(defmethod translate-instruction
    ((instruction cleavir-ir:return-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "RET"
    :operands '()))

(defmethod translate-instruction
    ((instruction cleavir-ir:funcall-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "CALL"
    :operands
    (list (translate-datum (first (cleavir-ir:inputs instruction))))))

(defmethod translate-instruction
    ((instruction cleavir-ir:catch-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "CALL"
    :operands
    (list (translate-datum (first (cleavir-ir:inputs instruction))))))

(defmethod translate-instruction
    ((instruction cleavir-ir:bind-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "CALL"
    :operands
    (list (translate-datum (first (cleavir-ir:inputs instruction))))))

(defmethod translate-instruction
    ((instruction cleavir-ir:unwind-instruction))
  (list (make-instance 'cluster:code-command
          :mnemonic "CALL"
          :operands
          (list (translate-datum (first (cleavir-ir:inputs instruction)))))
        (make-instance 'cluster:code-command
          :mnemonic "JMP"
          :operands
          (list (translate-datum sicl-mir-to-lir:*rax*)))))

(defmethod translate-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "CALL"
    :operands
    (list (translate-datum (first (cleavir-ir:inputs instruction))))))
