(cl:in-package #:sicl-hir)

;;; This instruction has two inputs and no outputs.  The first input
;;; is a register containing a cell as created by the
;;; MAKE-CELL-INSTRUCTION.  The second input is a register or a
;;; literal.  The result of executing this instruction is that the
;;; cell will contain the object in the second input.

(defclass write-cell-instruction (instruction)
  ())
