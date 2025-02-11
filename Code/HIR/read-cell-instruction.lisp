(cl:in-package #:sicl-hir)

;;; This instruction has one input and one output.  The input is a
;;; register containing a cell, as created by the
;;; MAKE-CELL-INSTRUCTION.  The output is a register that will contain
;;; the contents of the cell.

(defclass read-cell-instruction (instruction)
  ())
