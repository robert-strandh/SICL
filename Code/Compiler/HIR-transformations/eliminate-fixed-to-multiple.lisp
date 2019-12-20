(cl:in-package #:sicl-hir-transformations)

(defun eliminate-fixed-to-multiple-instruction (instruction)
  (let ((inputs (cleavir-ir:inputs instruction))
        (dynamic-environment-location
          (cleavir-ir:dynamic-environment-location instruction)))
    (if (null inputs)
        (cleavir-ir:insert-instruction-after
         (make-instance 'cleavir-ir:set-return-value-instruction
           :inputs (list (make-instance 'cleavir-ir:constant-input :value 0)
                         (make-instance 'cleavir-ir:constant-input :value nil))
           :outputs '()
           :dynamic-environment-location dynamic-environment-location)
         instruction)
        (loop for input in (reverse inputs)
              for i downfrom (1- (length inputs))
              do (cleavir-ir:insert-instruction-after
                  (make-instance 'cleavir-ir:set-return-value-instruction
                    :inputs (list (make-instance 'cleavir-ir:constant-input :value i)
                                  input)
                    :outputs '()
                    :dynamic-environment-location dynamic-environment-location)
                  instruction)))
    (change-class instruction 'cleavir-ir:initialize-return-values-instruction
                  :inputs (list (make-instance 'cleavir-ir:constant-input :value (length inputs)))
                  :outputs '())))

(defun eliminate-fixed-to-multiple-instructions (top-level-enter-instruction)
  (let ((fixed-to-multiple-instructions '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:fixed-to-multiple-instruction)
         (push instruction fixed-to-multiple-instructions)))
     top-level-enter-instruction)
    (loop for instruction in fixed-to-multiple-instructions
          do (eliminate-fixed-to-multiple-instruction instruction))))

