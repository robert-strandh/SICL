(cl:in-package #:sicl-mir-to-lir)

;;; At this point in register allocation, we nearly have a LIR graph.
;;; However, some instructions need to be introduced for:
;;; - calling unnamed functions
;;; - argument and return values

(defgeneric finish-lir-for-instruction (instruction))

;;; Most instructions don't need any final touches.

(defmethod finish-lir-for-instruction ((instruction cleavir-ir:instruction))
  nil)

(defun finish-lir (initial-instruction)
  (cleavir-ir:map-local-instructions
   #'finish-lir-for-instruction
   initial-instruction))
