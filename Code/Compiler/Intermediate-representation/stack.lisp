(cl:in-package #:sicl-ir)

;;; This instruction returns the stack pointer of the function calling
;;; the current one.
;;;
;;; It has no inputs, a single output, and a single successor.
(defclass caller-stack-pointer-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

;;; This instruction returns the frame pointer of the function calling
;;; the current one.
;;;
;;; It has no inputs, a single output, and a single successor.
(defclass caller-frame-pointer-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

;;; This instruction establishes a stack frame by setting the stack
;;; pointer and the frame pointer according to the inputs.
;;;
;;; It has two inputs, no outputs, and a single successor.
(defclass establish-stack-frame-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

;;; This instruction pushes a value on the stack.
;;;
;;; It has a single input, no outputs, and a single successor.
(defclass push-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

;;; This instruction pops a value from the stack.
;;;
;;; It has no inputs, a single output, and a single successor.
(defclass pop-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())
