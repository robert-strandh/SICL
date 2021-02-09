(cl:in-package #:sicl-hir-to-mir)

(defun expand-one-funcall-instruction (instruction)
  (let ((entry-point-offset-input
          (make-instance 'cleavir-ir:constant-input :value 2))
        (entry-point-location
          (make-instance 'cleavir-ir:lexical-location
            :name "ENTRY"))
        (static-environment-offset-input
          (make-instance 'cleavir-ir:constant-input :value 3))
        (static-environment-location
          (make-instance 'cleavir-ir:lexical-location
            :name "ENV"))
        (dynamic-environment-location
          (cleavir-ir:dynamic-environment-location instruction))
        (function-location (first (cleavir-ir:inputs instruction))))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:nook-read-instruction
       :inputs (list function-location entry-point-offset-input)
       :output entry-point-location)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:nook-read-instruction
       :inputs (list function-location static-environment-offset-input)
       :output static-environment-location)
     instruction)
    (setf (cleavir-ir:inputs instruction)
          (list* entry-point-location
                 static-environment-location
                 dynamic-environment-location
                 (rest (cleavir-ir:inputs instruction))))))

(defun expand-funcall-instructions (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (or (typep instruction 'cleavir-ir:funcall-instruction)
               (typep instruction 'cleavir-ir:bind-instruction)
               (typep instruction 'cleavir-ir:initialize-values-instruction))
       (expand-one-funcall-instruction instruction)))
   top-level-enter-instruction))
