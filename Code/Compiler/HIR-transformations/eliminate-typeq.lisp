(cl:in-package #:sicl-hir-transformations)

(defun eliminate-typeq-instruction (instruction)
  (let ((type (cleavir-ir:value-type instruction)))
    (cond ((and (subtypep type 'fixnum) (subtypep 'fixnum type))
           (change-class instruction 'cleavir-ir:fixnump-instruction))
          ((and (subtypep type 'cons) (subtypep 'cons type))
           (change-class instruction 'cleavir-ir:consp-instruction))
          ((and (subtypep type 'character) (subtypep 'character type))
           (change-class instruction 'cleavir-ir:characterp-instruction))
          ((and (subtypep type 'single-float) (subtypep 'single-float type))
           (change-class instruction 'cleavir-ir:single-float-p-instruction))
          ((and (subtypep type 'standard-object) (subtypep 'standard-object type))
           (change-class instruction 'cleavir-ir:standard-object-p-instruction))
          (t
           (let* ((object (first (cleavir-ir:inputs instruction)))
                  (typep-constant (cleavir-ir:make-constant-input 'typep))
                  (typep-function (cleavir-ir:new-temporary))
                  (type-descriptor-constant (cleavir-ir:make-constant-input type))
                  (boolean-value (cleavir-ir:new-temporary))
                  (nil-constant (cleavir-ir:make-constant-input 'nil)))
             (cleavir-ir:insert-instruction-before
              (make-instance 'cleavir-ir:fdefinition-instruction
                :input typep-constant
                :output typep-function)
              instruction)
             (cleavir-ir:insert-instruction-before
              (make-instance 'cleavir-ir:funcall-instruction
                :inputs (list typep-function object type-descriptor-constant))
              instruction)
             (cleavir-ir:insert-instruction-before
              (make-instance 'cleavir-ir:return-value-instruction 
                :input (make-instance 'cleavir-ir:constant-input
                         :value 0)
                :output boolean-value)
              instruction)
             (change-class instruction 'cleavir-ir:eq-instruction
                           :inputs (list boolean-value nil-constant)
                           :successors
                           (reverse (cleavir-ir:successors instruction))))))))

(defun eliminate-typeq-instructions (initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'cleavir-ir:typeq-instruction)
       (eliminate-typeq-instruction instruction)))
   initial-instruction))
