(cl:in-package #:sicl-hir-to-mir)

;;; Collect a list of ENTER-INSTRUCTIONs such that a parent
;;; ENTER-INSTRUCTION precedes all of its children in the list.
(defun gather-enter-instructions (enter-instruction)
  (let ((children (cleavir-ir:local-instructions-of-type
                   enter-instruction 'cleavir-ir:enter-instruction)))
    (cons enter-instruction
          (loop for child in children
                append (gather-enter-instructions child)))))
