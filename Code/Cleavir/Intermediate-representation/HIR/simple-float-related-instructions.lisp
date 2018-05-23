(cl:in-package #:cleavir-ir)

(defmacro define-simple-one-arg-float-instruction (name make-name)
  `(progn
     (defclass ,name (instruction one-successor-mixin)
       ((%subtype :initarg :subtype :reader subtype)))
     (defun ,make-name (input output successor)
       (make-instance ',name
         :inputs (list input)
         :outputs (list output)
         :successors (list successor)))
     (defmethod clone-initargs append ((instruction ,name))
       (list :subtype (subtype instruction)))))

(defmacro define-simple-two-arg-float-instruction (name make-name)
  `(progn
     (defclass ,name (instruction one-successor-mixin)
       ((%subtype :initarg :subtype :reader subtype)))
     (defun ,make-name (input1 input2 output successor)
       (make-instance ',name
         :inputs (list input1 input2)
         :outputs (list output)
         :successors (list successor)))
     (defmethod clone-initargs append ((instruction ,name))
       (list :subtype (subtype instruction)))))

(defmacro define-simple-float-comparison-instruction (name make-name)
  `(progn
     (defclass ,name (instruction two-successors-mixin)
       ((%subtype :initarg :subtype :reader subtype)))
     (defun ,make-name (input1 input2 successor1 successor2)
       (make-instance ',name
         :inputs (list input1 input2)
         :outputs '()
         :successors (list successor1 successor2)))
     (defmethod clone-initargs append ((instruction ,name))
       (list :subtype (subtype instruction)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION COERCE-INSTRUCTION
;;;
;;; This instruction takes a single input which must be an unboxed
;;; value of its FROM-TYPE. It has a single output which is an unboxed
;;; value of its TO-TYPE with the same mathematical value as the input.

(defclass coerce-instruction
    (instruction one-successor-mixin)
  ((%from-type :initarg :from :reader from-type)
   (%to-type :initarg :to :reader to-type)))

(defun make-coerce-instruction
    (from to input output successor)
  (make-instance 'coerce-instruction
    :from from :to to
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

(defmethod clone-initargs append ((instruction coerce-instruction))
  (list :from-type (from-type instruction) :to-type (to-type instruction)))
