(cl:in-package #:cleavir-ir)

(defmacro define-simple-one-arg-float-instruction (name make-name)
  `(progn
     (defclass ,name (instruction one-successor-mixin)
       ((%subtype :initarg :subtype :reader subtype)))
     (defun ,make-name (input output successor)
       (make-instance ',name
         :inputs (list input)
         :outputs (list output)
         :successors (list successor)))))

(defmacro define-simple-two-arg-float-instruction (name make-name)
  `(progn
     (defclass ,name (instruction one-successor-mixin)
       ((%subtype :initarg :subtype :reader subtype)))
     (defun ,make-name (input1 input2 output successor)
       (make-instance ',name
         :inputs (list input1 input2)
         :outputs (list output)
         :successors (list successor)))))

(defmacro define-simple-float-comparison-instruction (name make-name)
  `(progn
     (defclass ,name (instruction two-successors-mixin)
       ((%subtype :initarg :subtype :reader subtype)))
     (defun ,make-name (input1 input2 successor1 successor2)
       (make-instance ',name
         :inputs (list input1 input2)
         :outputs '()
         :successors (list successor1 successor2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-ADD-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be
;;; unboxed elements of the given SUBTYPE. Its single
;;; unboxed output is of that same type as well.

(define-simple-two-arg-float-instruction float-add-instruction make-float-add-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-SUB-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be
;;; unboxed elements of the given SUBTYPE. Its single
;;; unboxed output is of that same type as well.

(define-simple-two-arg-float-instruction float-sub-instruction make-float-sub-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-MUL-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be
;;; unboxed elements of the given SUBTYPE. Its single
;;; unboxed output is of that same type as well.

(define-simple-two-arg-float-instruction float-mul-instruction make-float-mul-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-DIV-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be
;;; unboxed elements of the given SUBTYPE. Its single
;;; unboxed output is of that same type as well.

(define-simple-two-arg-float-instruction float-div-instruction make-float-div-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-LESS-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be
;;; unboxed elements of the given SUBTYPE. It has
;;; no outputs. It has two successors; the first input
;;; is chosen when the first input is strictly less
;;; than the second one, otherwise the second
;;; successor is chosen.

(define-simple-float-comparison-instruction float-less-instruction make-float-less-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-NOT-GREATER-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be
;;; unboxed elements of the given SUBTYPE. It has
;;; no outputs. It has two successors; the first input
;;; is chosen when the first input is less than or
;;; equal to the second one, otherwise the second
;;; successor is chosen.

(define-simple-float-comparison-instruction float-not-greater-instruction make-float-not-greater-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-EQUAL-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be
;;; unboxed elements of the given SUBTYPE. It has
;;; no outputs. It has two successors; the first input
;;; is chosen when the first input is equal to the
;;; second one, otherwise the second
;;; successor is chosen.

(define-simple-float-comparison-instruction float-equal-instruction make-float-equal-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-SIN-INSTRUCTION.
;;;
;;; This instruction takes one input which must be
;;; an unboxed element of the given SUBTYPE. Its one
;;; output is an unboxed element of the same type,
;;; the sine of the input.

(define-simple-one-arg-float-instruction float-sin-instruction make-float-sin-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-COS-INSTRUCTION.
;;;
;;; This instruction takes one input which must be
;;; an unboxed element of the given SUBTYPE. Its one
;;; output is an unboxed element of the same type,
;;; the square root of the input.

(define-simple-one-arg-float-instruction float-cos-instruction make-float-cos-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-SQRT-INSTRUCTION.
;;;
;;; This instruction takes one input which must be
;;; an unboxed element of the given SUBTYPE. Its one
;;; output is an unboxed element of the same type,
;;; the square root of the input.

(define-simple-one-arg-float-instruction float-sqrt-instruction make-float-sqrt-instruction)
