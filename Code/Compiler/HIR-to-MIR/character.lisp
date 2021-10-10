(cl:in-package #:sicl-hir-to-mir)

(defmethod process-instruction
    (client (instruction cleavir-ir:char-code-instruction))
  (let ((shift-count (make-instance 'cleavir-ir:immediate-input :value 4)))
    (change-class instruction 'cleavir-ir:logic-shift-right-instruction
                  :inputs (append (cleavir-ir:inputs instruction)
                                  (list shift-count)))))

(defmethod process-instruction
    (client (instruction cleavir-ir:code-char-instruction))
  (let ((shift-count (make-instance 'cleavir-ir:immediate-input :value 4))
        (tag (make-instance 'cleavir-ir:immediate-input :value 3))
        (temp (make-instance 'cleavir-ir:lexical-location :name (gensym))))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:shift-left-instruction
       :inputs (append (cleavir-ir:inputs instruction) (list shift-count))
       :output temp)
     instruction)
    (change-class instruction 'cleavir-ir:bitwise-or-instruction
                  :inputs (list temp tag))))
