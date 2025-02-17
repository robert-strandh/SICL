(cl:in-package #:sicl-hir)

;;; This instruction is used as a successor of one or more
;;; UNWIND-INSTRUCTIONs when the exit point was created from a BLOCK.
;;; The successor of this instruction is the same as the successor of
;;; the last instruction created from the body of the BLOCK.  It has a
;;; single output that contains the value(s) transmitted by the
;;; UNWIND-INSTRUCTIONs created from RETURN-FROM.

(defclass receive-instruction (instruction)
  ())

