(cl:in-package #:sicl-hir-to-cl)

(defun find-lexical-locations (enter-instruction)
  (let ((result '()))
    (cleavir-ir:map-local-instructions
     (lambda (instruction)
       (loop for input in (cleavir-ir:inputs instruction)
             when (typep input 'cleavir-ir:lexical-location)
               do (pushnew input result))
       (loop for output in (cleavir-ir:outputs instruction)
             when (typep output 'cleavir-ir:lexical-location)
               do (pushnew output result)))
     enter-instruction)
    result))
