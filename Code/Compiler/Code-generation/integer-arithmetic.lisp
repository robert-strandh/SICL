(cl:in-package #:sicl-code-generation)


(defmethod translate-instruction
    ((instruction cleavir-ir:signed-add-instruction))
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
                :mnemonic "JOF"
                :operands
                (list
                 (find-instruction-label (second (cleavir-ir:successors instruction)))))))))

(defmethod translate-instruction
    ((instruction cleavir-ir:signed-sub-instruction))
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
                :mnemonic "JOF"
                :operands
                (list
                 (find-instruction-label (second (cleavir-ir:successors instruction)))))))))

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
    ((instruction cleavir-ir:equal-instruction))
  (list (make-instance 'cluster:code-command
          :mnemonic "CMP"
          :operands
          (list
           (translate-datum (first (cleavir-ir:inputs instruction)))
           (translate-datum (second (cleavir-ir:inputs instruction)))))
        (make-instance 'cluster:code-command
          :mnemonic "JNE"
          :operands
          (list
           (find-instruction-label (second (cleavir-ir:successors instruction)))))))

(defmethod translate-instruction
    ((instruction cleavir-ir:eq-instruction))
  (list (make-instance 'cluster:code-command
          :mnemonic "CMP"
          :operands
          (list
           (translate-datum (first (cleavir-ir:inputs instruction)))
           (translate-datum (second (cleavir-ir:inputs instruction)))))
        (make-instance 'cluster:code-command
          :mnemonic "JNE"
          :operands
          (list
           (find-instruction-label (second (cleavir-ir:successors instruction)))))))
