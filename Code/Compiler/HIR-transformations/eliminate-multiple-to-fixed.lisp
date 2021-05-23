(cl:in-package #:sicl-hir-transformations)

(defun eliminate-multiple-to-fixed-instruction (instruction)
  (let ((outputs (cleavir-ir:outputs instruction))
        (dynamic-environment-location
          (cleavir-ir:dynamic-environment-location instruction)))
    (cond ((null outputs)
           (change-class instruction 'cleavir-ir:nop-instruction))
          ((= (length outputs) 1)
           (change-class instruction 'cleavir-ir:return-value-instruction
                         :inputs (list (make-instance 'cleavir-ir:constant-input
                                         :value 0))
                         :outputs outputs
                         :dynamic-environment-location dynamic-environment-location))
          (t
           (let* ((true-branch
                    (make-instance 'cleavir-ir:return-value-instruction
                      :input (make-instance 'cleavir-ir:constant-input :value 0)
                      :output (first outputs)
                      :dynamic-environment-location dynamic-environment-location
                      :successor (cleavir-ir:first-successor instruction)))
                  (false-branch true-branch)
                  (count-location (make-instance 'cleavir-ir:lexical-location
                                    :name "RVC")))
             (loop for output in (rest outputs)
                   for i from 1
                   do (setf true-branch
                            (make-instance 'cleavir-ir:return-value-instruction
                              :input (make-instance 'cleavir-ir:constant-input
                                       :value i)
                              :output output
                              :dynamic-environment-location dynamic-environment-location
                              :successor true-branch))
                      (setf false-branch
                            (make-instance 'cleavir-ir:assignment-instruction
                              :input (make-instance 'cleavir-ir:constant-input
                                       :value nil)
                              :output output
                              :dynamic-environment-location dynamic-environment-location
                              :successor false-branch))
                      (setf false-branch
                            (make-instance 'cleavir-ir:fixnum-less-instruction
                              :inputs (list (make-instance 'cleavir-ir:constant-input
                                              :value i)
                                            count-location)
                              :dynamic-environment-location dynamic-environment-location
                              :successors (list true-branch false-branch))))
             (change-class instruction 'cleavir-ir:compute-return-value-count-instruction
                           :outputs (list count-location)
                           :successors (list false-branch)))))))

(defun eliminate-multiple-to-fixed-instructions (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'cleavir-ir:multiple-to-fixed-instruction)
       (eliminate-multiple-to-fixed-instruction instruction)))
   top-level-enter-instruction))
