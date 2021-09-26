(cl:in-package #:sicl-code-generation)

(defun compute-branches (instruction next mnemonic1 mnemonic2)
  (let ((successors (cleavir-ir:successors instruction)))
    (cond ((eq (first successors) next)
           (list (make-instance 'cluster:code-command
                   :mnemonic mnemonic1
                   :operands
                   (list
                    (find-instruction-label (second successors))))))
          ((eq (second successors) next)
           (list (make-instance 'cluster:code-command
                   :mnemonic mnemonic2
                   :operands
                   (list
                    (find-instruction-label (first successors))))))
          (t
           (list (make-instance 'cluster:code-command
                   :mnemonic mnemonic1
                   :operands
                   (list
                    (find-instruction-label (second successors))))
                 (make-instance 'cluster:code-command
                   :mnemonic "JMP"
                   :operands
                   (list
                    (find-instruction-label (first successors)))))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:signed-add-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "ADD"
      :operands
      (list
       (translate-datum destination)
       (translate-datum operand)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:signed-add-instruction) next)
  (cons
   (translate-simple-instruction instruction)
   (compute-branches instruction next "JOF" "JNO")))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:signed-sub-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "SUB"
      :operands
      (list
       (translate-datum destination)
       (translate-datum operand)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:signed-sub-instruction) next)
  (cons
   (translate-simple-instruction instruction)
   (compute-branches instruction next "JOF" "JNO")))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:negate-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "NEG"
    :operands
    (list (translate-datum
           (first (cleavir-ir:outputs instruction))))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:negate-instruction) next)
  (cons
   (translate-simple-instruction instruction)
   (compute-branches instruction next "JOF" "JNO")))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unsigned-add-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "ADD"
      :operands
      (list
       (translate-datum destination)
       (translate-datum operand)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:unsigned-add-instruction) next)
  (cons
   (translate-simple-instruction instruction)
   (compute-branches instruction next "JC" "JNC")))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unsigned-sub-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "SUB"
      :operands
      (list
       (translate-datum destination)
       (translate-datum operand)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:unsigned-sub-instruction) next)
  (cons
   (translate-simple-instruction instruction)
   (compute-branches instruction next "JC" "JNC")))

;;; FIXME: check this one.
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixnum-divide-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "DIV"
    :operands (list (translate-datum (second (cleavir-ir:inputs instruction))))))

(defun make-cmp (instruction)
  (let ((destination (first (cleavir-ir:inputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "CMP"
      :operands
      (list
       (translate-datum destination)
       (translate-datum operand)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:unsigned-less-instruction) next)
  (cons (make-cmp instruction)
        (compute-branches instruction next "JNB" "JB")))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:signed-less-instruction) next)
  (cons (make-cmp instruction)
        (compute-branches instruction next "JNL" "JL")))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:signed-not-greater-instruction) next)
  (cons (make-cmp instruction)
        (compute-branches instruction next "JG" "JNG")))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:equal-instruction) next)
  (cons (make-cmp instruction)
        (compute-branches instruction next "JNE" "JE")))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:eq-instruction) next)
  (cons (make-cmp instruction)
        (compute-branches instruction next "JNE" "JE")))
