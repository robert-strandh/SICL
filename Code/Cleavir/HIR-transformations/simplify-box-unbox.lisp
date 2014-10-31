(cl:in-package #:cleavir-hir-transformations)

;;;; We simplify boxing and unboxing in the following way: For each
;;;; unboxing instruction U, we check how the input was generated.  If
;;;; it was generated from a corresponding boxing instruction B, we
;;;; replace U with an assignment.  The assignment has the same output
;;;; as U, and the input of B.
;;;;
;;;; It is unlikely that there will be any pairs of an unboxing
;;;; instruction generating input for a boxing instruction, because
;;;; when there is some unboxing operation, it is in order to perform
;;;; some operation on the unboxed value, or to store it in some
;;;; specialized array.
;;;;
;;;; When all simplifications have been made, it is possible that
;;;; there are boxing instructions that generate outputs that are
;;;; never used.  Those boxing instructions are then replaced by NOP
;;;; instructions.

;;  LocalWords:  unboxing unboxed

