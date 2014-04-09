(in-package #:cleavir-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FIXNUM-+-INSTRUCTION.

(defclass fixnum-+-instruction (instruction two-successors-mixin)
  ())

(defun make-fixnum-+-instruction (inputs output successors)
  (make-instance 'fixnum-+-instruction
    :inputs inputs
    :outputs (list output)
    :successors successors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FIXNUM---INSTRUCTION.

(defclass fixnum---instruction (instruction two-successors-mixin)
  ())

(defun make-fixnum---instruction (inputs output successors)
  (make-instance 'fixnum---instruction
    :inputs inputs
    :outputs (list output)
    :successors successors))
