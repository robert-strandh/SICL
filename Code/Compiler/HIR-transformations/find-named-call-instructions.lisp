(cl:in-package #:sicl-hir-transformations)

(defun find-named-call-instructions (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'cleavir-ir:named-call-instruction)
       (push instruction (named-call-instructions top-level-enter-instruction))))
   top-level-enter-instruction))
