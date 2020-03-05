(cl:in-package #:sicl-hir-transformations)

(defgeneric preprocess-multiple-value-call-instruction (instruction))

(defmethod preprocess-multiple-value-call-instruction
    ((instruction t))
  nil)

(defmethod preprocess-multiple-value-call-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction))
  (let* ((call-function-constant-input
           (make-instance 'cleavir-ir:constant-input
             :value 'sicl-run-time:call-with-values))
         (call-function-location
           (make-instance 'cleavir-ir:lexical-location
             :name "call-function")))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fdefinition-instruction
       :input call-function-constant-input
       :output call-function-location)
     instruction)
    (push call-function-location
          (cleavir-ir:inputs instruction))))

(defun preprocess-multiple-value-call-instructions
    (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   #'preprocess-multiple-value-call-instruction top-level-enter-instruction))
