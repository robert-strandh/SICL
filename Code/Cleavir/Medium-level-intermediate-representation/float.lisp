(in-package #:cleavir-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SHORT-FLOAT-UNBOX-INSTRUCTION.
;;;
;;; This instruction takes a single input, which must be a boxed
;;; SHORT-FLOAT.  It has a single output which is the corresponding
;;; unboxed SHORT-FLOAT value.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type.

(defclass short-float-unbox-instruction (instruction one-successors-mixin)
  ())

(defun make-short-float-unbox-instruction (input output successor)
  (make-instance 'short-float-unbox-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SHORT-FLOAT-BOX-INSTRUCTION.
;;;
;;; This instruction takes a single input, which must be an unboxed
;;; SHORT-FLOAT.  It has a single output which is the corresponding
;;; boxed SHORT-FLOAT value.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type.

(defclass short-float-box-instruction (instruction one-successors-mixin)
  ())

(defun make-short-float-box-instruction (input output successor)
  (make-instance 'short-float-box-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SHORT-FLOAT-ADD-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SHORT-FLOAT.  It has a single output which is the
;;; unboxed SHORT-FLOAT sum of the two inputs.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type.

(defclass short-float-add-instruction (instruction one-successors-mixin)
  ())

(defun make-short-float-add-instruction (input1 input2 output successor)
  (make-instance 'short-float-add-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SHORT-FLOAT-SUB-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SHORT-FLOAT.  It has a single output which is the
;;; unboxed SHORT-FLOAT difference between the two inputs.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type.

(defclass short-float-sub-instruction (instruction one-successors-mixin)
  ())

(defun make-short-float-sub-instruction (input1 input2 output successor)
  (make-instance 'short-float-sub-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SHORT-FLOAT-MUL-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SHORT-FLOAT.  It has a single output which is the
;;; unboxed SHORT-FLOAT product of the two inputs.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type.

(defclass short-float-mul-instruction (instruction one-successors-mixin)
  ())

(defun make-short-float-mul-instruction (input1 input2 output successor)
  (make-instance 'short-float-mul-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SHORT-FLOAT-DIV-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SHORT-FLOAT.  It has a single output which is the
;;; unboxed SHORT-FLOAT quotient of the two inputs.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type.

(defclass short-float-div-instruction (instruction one-successors-mixin)
  ())

(defun make-short-float-div-instruction (input1 input2 output successor)
  (make-instance 'short-float-div-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SHORT-FLOAT-SIN-INSTRUCTION.
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed SHORT-FLOAT.  It has a single output which is the
;;; unboxed SHORT-FLOAT sine of the input.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type.

(defclass short-float-sin-instruction (instruction one-successors-mixin)
  ())

(defun make-short-float-sin-instruction (input output successor)
  (make-instance 'short-float-sin-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SHORT-FLOAT-COS-INSTRUCTION.
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed SHORT-FLOAT.  It has a single output which is the
;;; unboxed SHORT-FLOAT cosine of the input.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type.

(defclass short-float-cos-instruction (instruction one-successors-mixin)
  ())

(defun make-short-float-cos-instruction (input output successor)
  (make-instance 'short-float-cos-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SINGLE-FLOAT-UNBOX-INSTRUCTION.
;;;
;;; This instruction takes a single input, which must be a boxed
;;; SINGLE-FLOAT.  It has a single output which is the corresponding
;;; unboxed SINGLE-FLOAT value.

(defclass single-float-unbox-instruction (instruction one-successors-mixin)
  ())

(defun make-single-float-unbox-instruction (input output successor)
  (make-instance 'single-float-unbox-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SINGLE-FLOAT-BOX-INSTRUCTION.
;;;
;;; This instruction takes a single input, which must be an unboxed
;;; SINGLE-FLOAT.  It has a single output which is the corresponding
;;; boxed SINGLE-FLOAT value.

(defclass single-float-box-instruction (instruction one-successors-mixin)
  ())

(defun make-single-float-box-instruction (input output successor)
  (make-instance 'single-float-box-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SINGLE-FLOAT-ADD-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SINGLE-FLOAT.  It has a single output which is the
;;; unboxed SINGLE-FLOAT sum of the two inputs.

(defclass single-float-add-instruction (instruction one-successors-mixin)
  ())

(defun make-single-float-add-instruction (input1 input2 output successor)
  (make-instance 'single-float-add-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SINGLE-FLOAT-SUB-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SINGLE-FLOAT.  It has a single output which is the
;;; unboxed SINGLE-FLOAT difference between the two inputs.

(defclass single-float-sub-instruction (instruction one-successors-mixin)
  ())

(defun make-single-float-sub-instruction (input1 input2 output successor)
  (make-instance 'single-float-sub-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SINGLE-FLOAT-MUL-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SINGLE-FLOAT.  It has a single output which is the
;;; unboxed SINGLE-FLOAT product of the two inputs.

(defclass single-float-mul-instruction (instruction one-successors-mixin)
  ())

(defun make-single-float-mul-instruction (input1 input2 output successor)
  (make-instance 'single-float-mul-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SINGLE-FLOAT-DIV-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SINGLE-FLOAT.  It has a single output which is the
;;; unboxed SINGLE-FLOAT quotient of the two inputs.

(defclass single-float-div-instruction (instruction one-successors-mixin)
  ())

(defun make-single-float-div-instruction (input1 input2 output successor)
  (make-instance 'single-float-div-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SINGLE-FLOAT-SIN-INSTRUCTION.
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed SINGLE-FLOAT.  It has a single output which is the
;;; unboxed SINGLE-FLOAT sine of the input.
;;;
;;; This instruction can be used by implementations that support the
;;; SINGLE-FLOAT data type.

(defclass single-float-sin-instruction (instruction one-successors-mixin)
  ())

(defun make-single-float-sin-instruction (input output successor)
  (make-instance 'single-float-sin-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction DOUBLE-FLOAT-UNBOX-INSTRUCTION.
;;;
;;; This instruction takes a single input, which must be a boxed
;;; DOUBLE-FLOAT.  It has a single output which is the corresponding
;;; unboxed DOUBLE-FLOAT value.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type.

(defclass double-float-unbox-instruction (instruction one-successors-mixin)
  ())

(defun make-double-float-unbox-instruction (input output successor)
  (make-instance 'double-float-unbox-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction DOUBLE-FLOAT-BOX-INSTRUCTION.
;;;
;;; This instruction takes a single input, which must be an unboxed
;;; DOUBLE-FLOAT.  It has a single output which is the corresponding
;;; boxed DOUBLE-FLOAT value.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type.

(defclass double-float-box-instruction (instruction one-successors-mixin)
  ())

(defun make-double-float-box-instruction (input output successor)
  (make-instance 'double-float-box-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION DOUBLE-FLOAT-ADD-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed DOUBLE-FLOAT.  It has a single output which is the
;;; unboxed DOUBLE-FLOAT sum of the two inputs.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type.

(defclass double-float-add-instruction (instruction one-successors-mixin)
  ())

(defun make-double-float-add-instruction (input1 input2 output successor)
  (make-instance 'double-float-add-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION DOUBLE-FLOAT-SUB-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed DOUBLE-FLOAT.  It has a single output which is the
;;; unboxed DOUBLE-FLOAT difference between the two inputs.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type.

(defclass double-float-sub-instruction (instruction one-successors-mixin)
  ())

(defun make-double-float-sub-instruction (input1 input2 output successor)
  (make-instance 'double-float-sub-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION DOUBLE-FLOAT-MUL-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed DOUBLE-FLOAT.  It has a single output which is the
;;; unboxed DOUBLE-FLOAT product of the two inputs.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type.

(defclass double-float-mul-instruction (instruction one-successors-mixin)
  ())

(defun make-double-float-mul-instruction (input1 input2 output successor)
  (make-instance 'double-float-mul-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION DOUBLE-FLOAT-DIV-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed DOUBLE-FLOAT.  It has a single output which is the
;;; unboxed DOUBLE-FLOAT quotient of the two inputs.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type.

(defclass double-float-div-instruction (instruction one-successors-mixin)
  ())

(defun make-double-float-div-instruction (input1 input2 output successor)
  (make-instance 'double-float-div-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION DOUBLE-FLOAT-SIN-INSTRUCTION.
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed DOUBLE-FLOAT.  It has a single output which is the
;;; unboxed DOUBLE-FLOAT sine of the input.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type.

(defclass double-float-sin-instruction (instruction one-successors-mixin)
  ())

(defun make-double-float-sin-instruction (input output successor)
  (make-instance 'double-float-sin-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction LONG-FLOAT-UNBOX-INSTRUCTION.
;;;
;;; This instruction takes a single input, which must be a boxed
;;; LONG-FLOAT.  It has a single output which is the corresponding
;;; unboxed LONG-FLOAT value.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type.

(defclass long-float-unbox-instruction (instruction one-successors-mixin)
  ())

(defun make-long-float-unbox-instruction (input output successor)
  (make-instance 'long-float-unbox-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction LONG-FLOAT-BOX-INSTRUCTION.
;;;
;;; This instruction takes a single input, which must be an unboxed
;;; LONG-FLOAT.  It has a single output which is the corresponding
;;; boxed LONG-FLOAT value.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type.

(defclass long-float-box-instruction (instruction one-successors-mixin)
  ())

(defun make-long-float-box-instruction (input output successor)
  (make-instance 'long-float-box-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION LONG-FLOAT-ADD-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed LONG-FLOAT.  It has a single output which is the
;;; unboxed LONG-FLOAT sum of the two inputs.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type.

(defclass long-float-add-instruction (instruction one-successors-mixin)
  ())

(defun make-long-float-add-instruction (input1 input2 output successor)
  (make-instance 'long-float-add-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION LONG-FLOAT-SUB-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed LONG-FLOAT.  It has a single output which is the
;;; unboxed LONG-FLOAT difference between the two inputs.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type.

(defclass long-float-sub-instruction (instruction one-successors-mixin)
  ())

(defun make-long-float-sub-instruction (input1 input2 output successor)
  (make-instance 'long-float-sub-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION LONG-FLOAT-MUL-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed LONG-FLOAT.  It has a single output which is the
;;; unboxed LONG-FLOAT product of the two inputs.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type.

(defclass long-float-mul-instruction (instruction one-successors-mixin)
  ())

(defun make-long-float-mul-instruction (input1 input2 output successor)
  (make-instance 'long-float-mul-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION LONG-FLOAT-DIV-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed LONG-FLOAT.  It has a single output which is the
;;; unboxed LONG-FLOAT quotient of the two inputs.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type.

(defclass long-float-div-instruction (instruction one-successors-mixin)
  ())

(defun make-long-float-div-instruction (input1 input2 output successor)
  (make-instance 'long-float-div-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION LONG-FLOAT-SIN-INSTRUCTION.
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed LONG-FLOAT.  It has a single output which is the
;;; unboxed LONG-FLOAT sine of the input.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type.

(defclass long-float-sin-instruction (instruction one-successors-mixin)
  ())

(defun make-long-float-sin-instruction (input output successor)
  (make-instance 'long-float-sin-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

