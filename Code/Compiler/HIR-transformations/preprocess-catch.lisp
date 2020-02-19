(cl:in-package #:sicl-hir-transformations)

(defgeneric preprocess-catch-instruction (instruction))

(defmethod preprocess-catch-instruction
    ((instruction t))
  nil)

(defmethod preprocess-catch-instruction
    ((instruction cleavir-ir:catch-instruction))
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
    (setf (cleavir-ir:inputs instruction)
          (list augment-function-location))))

(defun preprocess-catch-instructions
    (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   #'preprocess-catch-instruction top-level-enter-instruction))
