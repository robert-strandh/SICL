(cl:in-package #:sicl-hir-transformations)

(defun make-initial-instructions
    (successor
     values-location
     value-count-location
     cell-location
     dynamic-environment-location)
  (make-instance 'cleavir-ir:compute-return-value-count-instruction
    :output value-count-location
    :dynamic-environment-location dynamic-environment-location
    :successor
    (make-instance 'cleavir-ir:car-instruction
      :input values-location
      :output cell-location
      :successor successor)))

(defun make-incrementation-instruction
    (successor index-location dynamic-environment-location)
  (make-instance 'cleavir-ir:unsigned-add-instruction
    :augend index-location
    :addend (make-instance 'cleavir-ir:constant-input
              :value 1)
    :dynamic-environment-location dynamic-environment-location
    :successor successor))

(defun make-final-instruction
    (successor values-location cell-location dynamic-environment-location)
  (make-instance 'cleavir-ir:rplaca-instruction
    :inputs (list values-location cell-location)
    :dynamic-environment-location dynamic-environment-location
    :successor successor))

(defun make-test-step
    (true-successor
     false-successor
     value-count-location
     index-input
     cell-location
     dynamic-environment-location)
  (let ((temp-location (make-instance 'cleavir-ir:lexical-location
                         :name (gensym "temp"))))
    (make-instance 'cleavir-ir:unsigned-less-instruction
      :inputs (list index-input value-count-location)
      :dynamic-environment-location dynamic-environment-location
      :successors
      (list
       (make-instance 'cleavir-ir:return-value-instruction
         :output temp-location
         :dynamic-environment-location dynamic-environment-location
         :successor
         (make-instance 'cleavir-ir:rplaca-instruction
           :inputs (list cell-location temp-location)
           :dynamic-environment-location dynamic-environment-location
           :successor
           (make-instance 'cleavir-ir:cdr-instruction
             :input cell-location
             :output cell-location
             :dynamic-environment-location dynamic-environment-location
             :successor true-successor)))
       false-successor))))

