(cl:in-package #:sicl-hir-transformations)

(defun eliminate-fetch-instruction (instruction)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (static-environment-input (first inputs))
         (index-input (second inputs))
         (index-value (cleavir-ir:value index-input))
         (new-index-input (make-instance 'cleavir-ir:constant-input
                            :value index-value)))
    (change-class instruction
                  'cleavir-ir:aref-instruction
                  :inputs (list static-environment-input new-index-input)
                  :boxed-p t
                  :simple-p t
                  :element-type t)))

(defun eliminate-fetch-instructions (initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'cleavir-ir:fetch-instruction)
       (eliminate-fetch-instruction instruction)))
   initial-instruction))
