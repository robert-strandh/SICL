(cl:in-package #:cleavir-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction BIT-UNBOX-INSTRUCTION.
;;;
;;; This instruction takes a single input, which must be a boxed BIT,
;;; i.e., FIXNUM with a value of 0 or 1.  It has a single output which
;;; is the corresponding unboxed BIT value.

(defclass bit-unbox-instruction
    (instruction one-successors-mixin unbox-instruction-mixin)
  ())

(defun make-bit-unbox-instruction (input output successor)
  (make-instance 'bit-unbox-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))
