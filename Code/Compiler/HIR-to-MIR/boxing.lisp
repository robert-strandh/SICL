(cl:in-package #:sicl-hir-to-mir)

(defun box-unsigned-integer (instruction)
  (let ((word-location (make-instance 'cleavir-ir:raw-integer :size 64))
        (shift-count-input (make-instance 'cleavir-ir:immediate-input :value 1)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:assignment-instruction
       :inputs (cleavir-ir:inputs instruction)
       :output word-location
       :successor instruction)
     instruction)
    (change-class instruction 'cleavir-ir:shift-left-instruction
                  :shifted-input word-location
                  :shift-count shift-count-input
                  :outputs (cleavir-ir:outputs instruction))))
