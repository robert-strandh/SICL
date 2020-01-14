(cl:in-package #:sicl-hir-transformations)

(defgeneric preprocess-catch-instruction (instruction))

(defmethod preprocess-catch-instruction
    ((instruction t))
  nil)

(defmethod preprocess-catch-instruction
    ((instruction cleavir-ir:catch-instruction))
  (let* ((make-instance-constant-input
           (make-instance 'cleavir-ir:constant-input
             :value 'make-instance))
         (make-instance-location
           (make-instance 'cleavir-ir:lexical-location
             :name "make-instance-function")))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fdefinition-instruction
       :input make-instance-constant-input
       :output make-instance-location)
     instruction)
    (setf (cleavir-ir:inputs instruction)
          (list make-instance-location))))

(defun preprocess-catch-instructions
    (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   #'preprocess-catch-instruction top-level-enter-instruction))

