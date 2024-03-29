(cl:in-package #:sicl-mir-to-lir)

;;; At this point in register allocation, we nearly have a LIR graph.
;;; However, some instructions need to be introduced for:
;;; - calling unnamed functions
;;; - argument and return values

(defgeneric finish-lir-for-instruction (instruction))

;;; Most instructions don't need any final touches.

(defmethod finish-lir-for-instruction ((instruction cleavir-ir:instruction))
  nil)

;;; The number of stack slots used for spilled lexical locations.
;;; This number is used to generate code for loading arguments from
;;; the stack.
(defvar *stack-slots*)
(defvar *spill-arguments-p*)

(defun finish-lir (initial-instruction stack-slots spill-arguments-p)
  (let ((*stack-slots* stack-slots)
        (*spill-arguments-p* spill-arguments-p))
    (cleavir-ir:map-local-instructions
     #'finish-lir-for-instruction
     initial-instruction)))
