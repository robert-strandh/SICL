(cl:in-package #:cleavir-ir)

(defmacro define-simple-one-arg-float-instruction (name)
  `(progn
     (defclass ,name (instruction one-successor-mixin)
       ((%subtype :initarg :subtype :reader subtype)))
     (defmethod clone-initargs append ((instruction ,name))
       (list :subtype (subtype instruction)))))

;;; While x86-64 specifies only two-address binary instructions, we
;;; intend to make use of three-address binary instructions provided
;;; by the AVX extension.
(defmacro define-simple-two-arg-float-instruction
    (name &key (additional-superclasses '()))
  `(progn
     (defclass ,name (instruction one-successor-mixin ,@additional-superclasses)
       ((%subtype :initarg :subtype :reader subtype)))
     (defmethod clone-initargs append ((instruction ,name))
       (list :subtype (subtype instruction)))))

(defmacro define-simple-float-comparison-instruction (name)
  `(progn
     (defclass ,name (instruction multiple-successors-mixin)
       ((%subtype :initarg :subtype :reader subtype)))
     (defmethod clone-initargs append ((instruction ,name))
       (list :subtype (subtype instruction)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-ADD-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be
;;; unboxed elements of the given SUBTYPE. Its single
;;; unboxed output is of that same type as well.

(define-simple-two-arg-float-instruction float-add-instruction
  :additional-superclasses (commutative-mixin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-SUB-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be
;;; unboxed elements of the given SUBTYPE. Its single
;;; unboxed output is of that same type as well.

(define-simple-two-arg-float-instruction float-sub-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-MUL-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be
;;; unboxed elements of the given SUBTYPE. Its single
;;; unboxed output is of that same type as well.

(define-simple-two-arg-float-instruction float-mul-instruction
  :additional-superclasses (commutative-mixin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-DIV-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be
;;; unboxed elements of the given SUBTYPE. Its single
;;; unboxed output is of that same type as well.

(define-simple-two-arg-float-instruction float-div-instruction)

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

(define-simple-float-comparison-instruction float-less-instruction)

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

(define-simple-float-comparison-instruction float-not-greater-instruction)

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

(define-simple-float-comparison-instruction float-equal-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-SIN-INSTRUCTION.
;;;
;;; This instruction takes one input which must be
;;; an unboxed element of the given SUBTYPE. Its one
;;; output is an unboxed element of the same type,
;;; the sine of the input.

(define-simple-one-arg-float-instruction float-sin-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-COS-INSTRUCTION.
;;;
;;; This instruction takes one input which must be
;;; an unboxed element of the given SUBTYPE. Its one
;;; output is an unboxed element of the same type,
;;; the square root of the input.

(define-simple-one-arg-float-instruction float-cos-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FLOAT-SQRT-INSTRUCTION.
;;;
;;; This instruction takes one input which must be
;;; an unboxed element of the given SUBTYPE. Its one
;;; output is an unboxed element of the same type,
;;; the square root of the input.

(define-simple-one-arg-float-instruction float-sqrt-instruction)

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

(defmethod clone-initargs append ((instruction coerce-instruction))
  (list :from-type (from-type instruction) :to-type (to-type instruction)))
