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
    ((instruction cleavir-ir:unsigned-less-instruction))
  (list (make-instance 'cluster:code-command
          :mnemonic "CMP"
          :operands
          (list
           (translate-datum (first (cleavir-ir:inputs instruction)))
           (translate-datum (second (cleavir-ir:inputs instruction)))))
        (make-instance 'cluster:code-command
          :mnemonic "JNB"
          :operands
          (list
           (find-instruction-label (second (cleavir-ir:successors instruction)))))))

(defmethod translate-instruction
    ((instruction cleavir-ir:signed-less-instruction))
  (list (make-instance 'cluster:code-command
          :mnemonic "CMP"
          :operands
          (list
           (translate-datum (first (cleavir-ir:inputs instruction)))
           (translate-datum (second (cleavir-ir:inputs instruction)))))
        (make-instance 'cluster:code-command
          :mnemonic "JNL"
          :operands
          (list
           (find-instruction-label (second (cleavir-ir:successors instruction)))))))

(defmethod translate-instruction
    ((instruction cleavir-ir:signed-not-greater-instruction))
  (list (make-instance 'cluster:code-command
          :mnemonic "CMP"
          :operands
          (list
           (translate-datum (first (cleavir-ir:inputs instruction)))
           (translate-datum (second (cleavir-ir:inputs instruction)))))
        (make-instance 'cluster:code-command
          :mnemonic "JG"
          :operands
          (list
           (find-instruction-label (second (cleavir-ir:successors instruction)))))))

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
    ((instruction cleavir-ir:unsigned-add-instruction))
  (assert (eq (first (cleavir-ir:inputs instruction))
              (first (cleavir-ir:outputs instruction))))
  (let ((successors (cleavir-ir:successors instruction))
        (add (make-instance 'cluster:code-command
               :mnemonic "ADD"
               :operands
               (list
                (translate-datum (first (cleavir-ir:inputs instruction)))
                (translate-datum (second (cleavir-ir:inputs instruction)))))))
    (if (or (= (length successors) 1)
            (eq (first successors) (second successors)))
        add
        (list add
              (make-instance 'cluster:code-command
                :mnemonic "JCF"
                :operands
                (list
                 (find-instruction-label (second (cleavir-ir:successors instruction)))))))))

(defmethod translate-instruction
    ((instruction cleavir-ir:unsigned-sub-instruction))
  (assert (eq (first (cleavir-ir:inputs instruction))
              (first (cleavir-ir:outputs instruction))))
  (let ((successors (cleavir-ir:successors instruction))
        (add (make-instance 'cluster:code-command
               :mnemonic "SUB"
               :operands
               (list
                (translate-datum (first (cleavir-ir:inputs instruction)))
                (translate-datum (second (cleavir-ir:inputs instruction)))))))
    (if (or (= (length successors) 1)
            (eq (first successors) (second successors)))
        add
        (list add
              (make-instance 'cluster:code-command
                :mnemonic "JCF"
                :operands
                (list
                 (find-instruction-label (second (cleavir-ir:successors instruction)))))))))

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
