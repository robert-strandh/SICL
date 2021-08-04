(cl:in-package #:sicl-mir-to-lir)

;;; At this point in register allocation, we nearly have a LIR graph.
;;; However, some instructions need to be introduced for: - calling
;;; unnamed functions.

(defgeneric finish-lir-for-instruction (instruction))

;;; Most instructions don't need any final touches.

(defmethod finish-lir-for-instruction ((instruction cleavir-ir:instruction))
  nil)

;;; A hack to remove pointless assignments.

(defmethod finish-lir-for-instruction ((instruction cleavir-ir:assignment-instruction))
  (when (eq (first (cleavir-ir:inputs instruction))
            (first (cleavir-ir:outputs instruction)))
    (cleavir-ir:delete-instruction instruction)))

(defun finish-lir (initial-instruction)
  (cleavir-ir:map-local-instructions
   #'finish-lir-for-instruction
   initial-instruction))
