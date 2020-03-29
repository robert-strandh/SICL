(cl:in-package #:sicl-hir-transformations)

(defun transform-fdefinition-instruction
    (top-level-enter-instruction fdefinition-instruction table)
  (let ((input (first (cleavir-ir:inputs fdefinition-instruction)))
        (static-environment-location
          (cleavir-ir:static-environment top-level-enter-instruction)))
    (assert (typep input 'cleavir-ir:constant-input))
    (let ((function-name (cleavir-ir:value input)))
      (with-accessors ((function-names function-names))
          top-level-enter-instruction
        (let ((pos (position function-name function-names :test #'equal)))
          (when (null pos)
            (setf pos (length function-names))
            (setf function-names (append function-names (list function-name)))
            (let ((temp (cleavir-ast-to-hir:make-temp)))
              (cleavir-ir:insert-instruction-after
               (make-instance 'cleavir-ir:fetch-instruction
                 :inputs (list static-environment-location
                          (make-instance 'cleavir-ir:constant-input
                            :value pos))
                 :output temp)
               top-level-enter-instruction)
              (setf (gethash function-name table) temp)))))
      (change-class fdefinition-instruction 'cleavir-ir:car-instruction
                    :inputs (list (gethash function-name table))))))

(defun find-fdefinition-instructions (top-level-enter-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:fdefinition-instruction)
         (push instruction result)))
     top-level-enter-instruction)
    result))

(defun hoist-fdefinitions (top-level-enter-instruction)
  (let ((fdefinition-instructions
          (find-fdefinition-instructions top-level-enter-instruction))
        (table (make-hash-table :test #'equal)))
    (loop for fdefinition-instruction in fdefinition-instructions
          do (transform-fdefinition-instruction
              top-level-enter-instruction fdefinition-instruction table))))
