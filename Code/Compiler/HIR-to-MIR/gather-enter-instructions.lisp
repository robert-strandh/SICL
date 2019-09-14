(cl:in-package #:sicl-hir-to-mir)

;;; Collect a list of ENTER-INSTRUCTIONs such that a parent
;;; ENTER-INSTRUCTION precedes all of its children in the list.
(defun gather-enter-instructions (enter-instruction)
  (let ((enclose-instructions
          (cleavir-ir:local-instructions-of-type
           enter-instruction 'cleavir-ir:enclose-instruction)))
    (cons enter-instruction
          (loop for enclose-instruction in enclose-instructions
                for child = (cleavir-ir:code enclose-instruction)
                append (gather-enter-instructions child)))))
