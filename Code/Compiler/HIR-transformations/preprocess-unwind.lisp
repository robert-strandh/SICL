(cl:in-package #:sicl-hir-transformations)

(defgeneric preprocess-unwind-instruction (instruction))

(defmethod preprocess-unwind-instruction
    ((instruction t))
  nil)

(defmethod preprocess-unwind-instruction
    ((instruction cleavir-ir:unwind-instruction))
  (let* ((augment-function-constant-input
           (make-instance 'cleavir-ir:constant-input
             :value 'sicl-run-time:unwind))
         (augment-function-location
           (make-instance 'cleavir-ir:lexical-location
             :name "augmentation-function")))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fdefinition-instruction
       :input augment-function-constant-input
       :output augment-function-location)
     instruction)
    (push augment-function-location
          (cleavir-ir:inputs instruction))))

(defun preprocess-unwind-instructions
    (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   #'preprocess-unwind-instruction top-level-enter-instruction))

