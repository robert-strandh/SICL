(cl:in-package #:sicl-hir-transformations)

(defun eliminate-create-cell-instruction (instruction owner)
  (let ((static-environment-location (cleavir-ir:static-environment owner))
        (dynamic-environment-location
          (cleavir-ir:dynamic-environment-location instruction))
        (nil-location
          (make-instance 'cleavir-ir:lexical-location :name (gensym "nil")))
        (cons-function-offset-input
          (make-instance 'cleavir-ir:constant-input :value -2))
        (nil-offset-input
          (make-instance 'cleavir-ir:constant-input :value -1))
        (cons-function-location
          (make-instance 'cleavir-ir:lexical-location
            :name (gensym "cons-function")))
        (values-location (make-instance 'cleavir-ir:values-location)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fetch-instruction
       :inputs (list static-environment-location cons-function-offset-input)
       :output cons-function-location
       :successor instruction
       :dynamic-environment-location dynamic-environment-location)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fetch-instruction
       :inputs (list static-environment-location nil-offset-input)
       :output nil-location
       :successor instruction
       :dynamic-environment-location dynamic-environment-location)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:funcall-instruction
       :inputs (list
                cons-function-location
                nil-location
                nil-location)
       :output values-location
       :successor instruction
       :dynamic-environment-location dynamic-environment-location)
     instruction)
    (change-class instruction 'cleavir-ir:multiple-to-fixed-instruction
                  :inputs (list values-location))))

(defun eliminate-create-cell-instructions (initial-instruction)
  (cleavir-ir:map-instructions-with-owner
   (lambda (instruction owner)
     (when (typep instruction 'cleavir-ir:create-cell-instruction)
       (eliminate-create-cell-instruction instruction owner)))
   initial-instruction))
