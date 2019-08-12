(cl:in-package #:sicl-hir-to-mir)

(defun process-create-cell-instruction (create-cell-instruction)
  (let ((dynamic-environment-location
          (cleavir-ir:dynamic-environment-location create-cell-instruction))
        (cons-function-cell-location 
          (make-instance 'cleavir-ir:lexical-location
            :name (gensym "cons-function-cell")))
        (cons-function-location 
          (make-instance 'cleavir-ir:lexical-location
            :name (gensym "cons-function")))
        (nil-location
          (make-instance 'cleavir-ir:lexical-location
            :name (gensym "nil")))
        (values-location
          (make-instance 'cleavir-ir:values-location))
        (cell-location
          (first (cleavir-ir:outputs create-cell-instruction))))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fetch-instruction
       :input (make-instance 'cleavir-ir:immediate-input
                :value (- *cons-function-cell-index* *fixed-position-count*))
       :output cons-function-cell-location
       :successor create-cell-instruction
       :dynamic-environment-location dynamic-environment-location)
     create-cell-instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:car-instruction
       :input cons-function-cell-location
       :output cons-function-location
       :successor create-cell-instruction
       :dynamic-environment-location dynamic-environment-location)
     create-cell-instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fetch-instruction
       :input (make-instance 'cleavir-ir:immediate-input
                :value (- *nil-index* *fixed-position-count*))
       :output nil-location
       :successor create-cell-instruction
       :dynamic-environment-location dynamic-environment-location)
     create-cell-instruction)
    (change-class create-cell-instruction
                  'cleavir-ir:funcall-instruction
                  :inputs (list nil-location nil-location)
                  :output values-location)
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:fixed-to-multiple-instruction
       :input values-location
       :output cell-location)
     create-cell-instruction)))
