(cl:in-package #:sicl-hir-to-mir)

(defun process-enclose-instruction (enclose-instruction)
  (let ((dynamic-environment-location
          (cleavir-ir:dynamic-environment-location enclose-instruction))
        (code-object-location
          (make-instance 'cleavir-ir:lexical-location
            :name (gensym "code-object")))
        (enclose-function-cell-location 
          (make-instance 'cleavir-ir:lexical-location
            :name (gensym "enclose-function-cell")))
        (enclose-function-location 
          (make-instance 'cleavir-ir:lexical-location
            :name (gensym "enclose-function")))
        (cons-function-cell-location 
          (make-instance 'cleavir-ir:lexical-location
            :name (gensym "cons-function-cell")))
        (cons-function-location 
          (make-instance 'cleavir-ir:lexical-location
            :name (gensym "cons-function")))
        ;; FIXME: do this better
        (entry-point-input
          (make-instance 'cleavir-ir:constant-input
            :value 0))
        (values-location
          (make-instance 'cleavir-ir:values-location))
        (function-location
          (first (cleavir-ir:outputs enclose-instruction))))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fetch-instruction
       :input (make-instance 'cleavir-ir:immediate-input
                :value (- *code-object-index* *fixed-position-count*))
       :output code-object-location
       :successor enclose-instruction
       :dynamic-environment-location dynamic-environment-location)
     enclose-instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fetch-instruction
       :input (make-instance 'cleavir-ir:immediate-input
                :value (- *enclose-function-cell-index* *fixed-position-count*))
       :output enclose-function-cell-location
       :successor enclose-instruction
       :dynamic-environment-location dynamic-environment-location)
     enclose-instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:car-instruction
       :input enclose-function-cell-location
       :output enclose-function-location
       :successor enclose-instruction
       :dynamic-environment-location dynamic-environment-location)
     enclose-instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fetch-instruction
       :input (make-instance 'cleavir-ir:immediate-input
                :value (- *cons-function-cell-index* *fixed-position-count*))
       :output cons-function-cell-location
       :successor enclose-instruction
       :dynamic-environment-location dynamic-environment-location)
     enclose-instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:car-instruction
       :input cons-function-cell-location
       :output cons-function-location
       :successor enclose-instruction
       :dynamic-environment-location dynamic-environment-location)
     enclose-instruction)
    (change-class enclose-instruction
                  'cleavir-ir:funcall-instruction
                  :inputs (list* entry-point-input
                                 code-object-location
                                 enclose-function-cell-location
                                 cons-function-cell-location
                                 (cleavir-ir:inputs enclose-instruction))
                  :output values-location)
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:fixed-to-multiple-instruction
       :input values-location
       :output function-location)
     enclose-instruction)))

(defun process-enclose-instructions  (top-level-enter-instruction)
  (cleavir-ir:map-instructions-with-owner
   (lambda (instruction owner)
     (when (and (typep instruction 'cleavir-ir:enclose-instruction)
                (not (eq owner top-level-enter-instruction)))
       (process-enclose-instruction instruction)))
   top-level-enter-instruction))
