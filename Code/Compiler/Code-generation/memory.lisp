(cl:in-package #:sicl-code-generation)

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memref1-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (source (first (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "MOV"
      :operands
      (list
       (translate-datum destination)
       (translate-datum source)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memset1-instruction))
  (let ((destination (first (cleavir-ir:inputs instruction)))
        (source (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "MOV"
      :operands
      (list
       (translate-datum destination)
       (translate-datum source)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memref2-instruction))
  (destructuring-bind (base-register-input offset-input)
      (cleavir-ir:inputs instruction)
    (make-instance 'cluster:code-command
      :mnemonic "MOV"
      :operands
      (list
       (translate-datum (first (cleavir-ir:outputs instruction)))
       (if (typep offset-input 'cleavir-ir:immediate-input)
           (make-instance 'cluster:memory-operand
             :base-register (translate-datum base-register-input)
             :displacement (cleavir-ir:value offset-input)
             :size 64)
           (make-instance 'cluster:memory-operand
             :base-register (translate-datum base-register-input)
             :index-register (translate-datum offset-input)
             :scale 8
             :size 64))))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memset2-instruction))
  (destructuring-bind (base-register-input offset-input value-input)
      (cleavir-ir:inputs instruction)
    (make-instance 'cluster:code-command
      :mnemonic "MOV"
      :operands
      (list
       (if (typep offset-input 'cleavir-ir:immediate-input)
           (make-instance 'cluster:memory-operand
             :base-register (translate-datum base-register-input)
             :displacement (cleavir-ir:value offset-input)
             :size 64)
           (make-instance 'cluster:memory-operand
             :base-register (translate-datum base-register-input)
             :index-register (translate-datum offset-input)
             :scale 8
             :size 64))
       (translate-datum value-input)))))

(defmethod translate-simple-instruction
    ((instruction sicl-ir:memref-effective-address-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (source (first (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "MOV"
      :operands
      (list
       (translate-datum destination)
       (translate-datum source)))))
