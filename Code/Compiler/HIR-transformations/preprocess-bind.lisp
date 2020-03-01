(cl:in-package #:sicl-hir-transformations)

(defgeneric preprocess-bind-instruction (instruction))

(defmethod preprocess-bind-instruction
    ((instruction t))
  nil)

(defmethod preprocess-bind-instruction
    ((instruction cleavir-ir:bind-instruction))
  (let* ((augment-function-constant-input
           (make-instance 'cleavir-ir:constant-input
             :value 'sicl-run-time:augment-with-block/tagbody-entry))
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

(defun preprocess-bind-instructions
    (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   #'preprocess-bind-instruction top-level-enter-instruction))
