(cl:in-package #:sicl-code-generation)

(defgeneric translate-simple-instruction (instruction))

(defgeneric translate-branch-instruction (instruction next))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction))
  '())

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unreachable-instruction))
  '())

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enter-instruction))
  '())

(defmethod translate-simple-instruction
    ((instruction sicl-ir:breakpoint-instruction))
  '())

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "MOV"
    :operands
    (list
     (translate-datum (first (cleavir-ir:outputs instruction)))
     (translate-datum (first (cleavir-ir:inputs instruction))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:shift-left-instruction))
  (assert (eq (first (cleavir-ir:inputs instruction))
              (first (cleavir-ir:outputs instruction))))
  (make-instance 'cluster:code-command
    :mnemonic "SHL"
    :operands
    (list
     (translate-datum (first (cleavir-ir:outputs instruction)))
     (translate-datum (second (cleavir-ir:inputs instruction))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:return-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "RET"
    :operands '()))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "CALL"
    :operands
    (list (translate-datum (first (cleavir-ir:inputs instruction))))))

;;; The label to which we jump is bogus, because it is going to be set
;;; by the call-site manager.  But we need to give some operand to
;;; make-instance, so we take what we have.
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:catch-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "JMP"
    :operands
    (find-instruction-label (first successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:catch-instruction) next)
  (let ((successors (cleavir-ir:successors instruction)))
    (cons (translate-simple-instruction instruction)
          (if (eq (first (cleavir-ir:successors instruction)) next)
              '()
              (list (make-instance 'cluster:code-command
                      :mnemonic "JMP"
                      :operands
                      (list (find-instruction-label (first successors)))))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:bind-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "CALL"
    :operands
    (list (translate-datum (first (cleavir-ir:inputs instruction))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unwind-instruction))
  (list (make-instance 'cluster:code-command
          :mnemonic "CALL"
          :operands
          (list (translate-datum (first (cleavir-ir:inputs instruction)))))
        (make-instance 'cluster:code-command
          :mnemonic "JMP"
          :operands
          (list (translate-datum sicl-mir-to-lir:*rax*)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "CALL"
    :operands
    (list (translate-datum (first (cleavir-ir:inputs instruction))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:initialize-values-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "CALL"
    :operands
    (list (translate-datum (first (cleavir-ir:inputs instruction))))))
