(cl:in-package #:sicl-hir-transformations)

(defun eliminate-create-cell-instruction (instruction owner)
  (let* ((static-environment-location (cleavir-ir:static-environment owner))
         (nil-location
           (make-instance 'cleavir-ir:lexical-location :name (gensym "nil")))
         (cons-function-offset
           (- sicl-compiler:+cons-function-index+
              sicl-compiler:+first-constant-index+))
         (cons-function-offset-input
           (make-instance 'cleavir-ir:constant-input
             :value cons-function-offset))
         (nil-offset
           (- sicl-compiler:+nil-index+
              sicl-compiler:+first-constant-index+))
         (nil-offset-input
           (make-instance 'cleavir-ir:constant-input
             :value nil-offset))
         (cons-function-location
           (make-instance 'cleavir-ir:lexical-location
             :name (gensym "consfun"))))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fetch-instruction
       :inputs (list static-environment-location cons-function-offset-input)
       :output cons-function-location
       :successor instruction)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fetch-instruction
       :inputs (list static-environment-location nil-offset-input)
       :output nil-location
       :successor instruction)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:funcall-instruction
       :inputs (list
                cons-function-location
                nil-location
                nil-location)
       :successor instruction)
     instruction)
    (change-class instruction 'cleavir-ir:return-value-instruction
                  :inputs (list (make-instance 'cleavir-ir:constant-input
                                  :value 0)))))

(defun eliminate-create-cell-instructions (initial-instruction)
  (cleavir-ir:map-instructions-with-owner
   (lambda (instruction owner)
     (when (typep instruction 'cleavir-ir:create-cell-instruction)
       (eliminate-create-cell-instruction instruction owner)))
   initial-instruction))
