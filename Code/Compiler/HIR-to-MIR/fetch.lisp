(cl:in-package #:sicl-hir-to-mir)

;;; We transform FETCH to AREF, which will be processed later.

(defun process-fetch (initial-instruction)
  (cleavir-ir:map-instructions-with-owner
   (lambda (instruction owner)
     (when (typep instruction 'cleavir-ir:fetch-instruction)
       (let* ((immediate-input (first (cleavir-ir:inputs instruction)))
              (value (cleavir-ir:value immediate-input))
              (static-environment-location (first (cleavir-ir:outputs owner))))
         (change-class instruction 'cleavir-ir:aref-instruction
                       :element-type t
                       :simple-p t
                       :boxed-p t
                       :inputs (list static-environment-location (+ value 2))))))
   initial-instruction))
