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

(defun box-signed-integer (instruction)
  (let ((word-location (make-instance 'cleavir-ir:raw-integer :size 64))
        (shift-count-input (make-instance 'cleavir-ir:immediate-input :value 1)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:sign-extend-instruction
       :inputs (cleavir-ir:inputs instruction)
       :output word-location
       :successor instruction)
     instruction)
    (change-class instruction 'cleavir-ir:shift-left-instruction
                  :shifted-input word-location
                  :shift-count shift-count-input
                  :outputs (cleavir-ir:outputs instruction))))

(defun box-single-float (instruction)
  (let ((word-location-1 (make-instance 'cleavir-ir:raw-integer :size 64))
        (word-location-2 (make-instance 'cleavir-ir:raw-integer :size 64))
        (shift-count-input (make-instance 'cleavir-ir:immediate-input :value 32))
        (tag-input (make-instance 'cleavir-ir:immediate-input :value #b10011)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:assignment-instruction
       :inputs (cleavir-ir:inputs instruction)
       :output word-location-1
       :successor instruction)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:shift-left-instruction
       :shifted-input word-location-1
       :shift-count shift-count-input
       :output word-location-2
       :successor instruction)
     instruction)
    (change-class instruction 'cleavir-ir:bitwise-or-instruction
                  :inputs (list word-location-2 tag-input)
                  :outputs (cleavir-ir:outputs instruction))))
