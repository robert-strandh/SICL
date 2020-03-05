(cl:in-package #:sicl-hir-transformations)

(defgeneric preprocess-initialize-values-instruction (instruction))

(defmethod preprocess-initialize-values-instruction
    ((instruction t))
  nil)

(defmethod preprocess-initialize-values-instruction
    ((instruction cleavir-ir:initialize-values-instruction))
  (let* ((initialize-function-constant-input
           (make-instance 'cleavir-ir:constant-input
             :value 'sicl-run-time:initialize-values))
         (initialize-function-location
           (make-instance 'cleavir-ir:lexical-location
             :name "initialize-function")))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fdefinition-instruction
       :input initialize-function-constant-input
       :output initialize-function-location)
     instruction)
    (push initialize-function-location
          (cleavir-ir:inputs instruction))))

(defun preprocess-initialize-values-instructions
    (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   #'preprocess-initialize-values-instruction top-level-enter-instruction))
