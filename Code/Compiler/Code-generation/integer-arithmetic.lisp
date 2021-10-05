(cl:in-package #:sicl-code-generation)

(defun compute-branches (successors next mnemonic1 mnemonic2)
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
                  (find-instruction-label (first successors))))))))

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
   (compute-branches (cleavir-ir:successors instruction) next "JOF" "JNO")))

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
   (compute-branches (cleavir-ir:successors instruction) next "JOF" "JNO")))

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
   (compute-branches (cleavir-ir:successors instruction) next "JOF" "JNO")))

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
   (compute-branches (cleavir-ir:successors instruction) next "JC" "JNC")))

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
   (compute-branches (cleavir-ir:successors instruction) next "JC" "JNC")))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixnum-multiply-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "MUL"
    :operands (list (translate-datum (second (cleavir-ir:inputs instruction))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixnum-divide-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "DIV"
    :operands (list (translate-datum (second (cleavir-ir:inputs instruction))))))

(defun make-cmp (inputs)
  (let ((destination (first inputs))
        (operand (second inputs)))
    (make-instance 'cluster:code-command
      :mnemonic "CMP"
      :operands
      (list
       (translate-datum destination)
       (translate-datum operand)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:unsigned-less-instruction) next)
  (let ((successors (cleavir-ir:successors instruction))
        (inputs (cleavir-ir:inputs instruction)))
    (if (typep (first inputs) 'cleavir-ir:immediate-input)
        (cons (make-cmp (reverse inputs))
              (compute-branches successors next "JB" "JNB"))
        (cons (make-cmp inputs)
              (compute-branches successors next "JNB" "JB")))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:signed-less-instruction) next)
  (let ((successors (cleavir-ir:successors instruction))
        (inputs (cleavir-ir:inputs instruction)))
    (if (typep (first inputs) 'cleavir-ir:immediate-input)
        (cons (make-cmp (reverse inputs))
              (compute-branches successors next "JL" "JNL"))
        (cons (make-cmp inputs)
              (compute-branches successors next "JNL" "JL")))))

;;; This method is invoked when the SIGNED-NOT-GREATER instruction has
;;; been simplified because its two successors are the same.  So we
;;; generate no code for it.  FIXME: But it should have been
;;; eliminated before.
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:signed-not-greater-instruction))
  '())

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:signed-not-greater-instruction) next)
  (let ((successors (cleavir-ir:successors instruction))
        (inputs (cleavir-ir:inputs instruction)))
    (if (typep (first inputs) 'cleavir-ir:immediate-input)
        (cons (make-cmp (reverse inputs))
              (compute-branches successors next "JNG" "JG"))
        (cons (make-cmp inputs)
              (compute-branches successors next "JG" "JNG")))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:equal-instruction) next)
  (let ((successors (cleavir-ir:successors instruction))
        (inputs (cleavir-ir:inputs instruction)))
    (if (typep (first inputs) 'cleavir-ir:immediate-input)
        (cons (make-cmp (reverse inputs))
              (compute-branches successors next "JE" "JNE"))
        (cons (make-cmp inputs)
              (compute-branches successors next "JNE" "JE")))))

;;; This method is invoked when the EQ instruction has been simplified
;;; because its two successors are the same.  So we generate no code
;;; for it.  FIXME: But it should have been eliminated before.
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:eq-instruction))
  '())

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:eq-instruction) next)
  (let ((successors (cleavir-ir:successors instruction))
        (inputs (cleavir-ir:inputs instruction)))
    (if (typep (first inputs) 'cleavir-ir:immediate-input)
        (cons (make-cmp (reverse inputs))
              (compute-branches successors next "JE" "JNE"))
        (cons (make-cmp inputs)
              (compute-branches successors next "JNE" "JE")))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:bitwise-and-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "AND"
      :operands
      (list
       (translate-datum destination)
       (translate-datum operand)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:bitwise-or-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "OR"
      :operands
      (list
       (translate-datum destination)
       (translate-datum operand)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:bitwise-exclusive-or-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "XOR"
      :operands
      (list
       (translate-datum destination)
       (translate-datum operand)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:sign-extend-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (first (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "MOVSXD"
      :operands
      (list
       (translate-datum destination)
       (translate-datum operand)))))
