(cl:in-package #:cleavir-ir)

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

(defclass short-float-add-instruction (instruction one-successor-mixin)
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

(defclass short-float-sub-instruction (instruction one-successor-mixin)
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

(defclass short-float-mul-instruction (instruction one-successor-mixin)
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

(defclass short-float-div-instruction (instruction one-successor-mixin)
  ())

(defun make-short-float-div-instruction (input1 input2 output successor)
  (make-instance 'short-float-div-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SHORT-FLOAT-LESS-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SHORT-FLOAT.  It has no outputs.  It has two successors;
;;; the first one is chosen when the first input is strictly less than
;;; the second one, otherwise the second successor is chosen.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type.

(defclass short-float-less-instruction (instruction two-successors-mixin)
  ())

(defun make-short-float-less-instruction (input1 input2 successor1 successor2)
  (make-instance 'short-float-less-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor1 successor2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SHORT-FLOAT-NOT-GREATER-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SHORT-FLOAT.  It has no outputs.  It has two successors;
;;; the first one is chosen when the first input is less than or equal
;;; to the second one, otherwise the second successor is chosen.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type.

(defclass short-float-not-greater-instruction (instruction two-successors-mixin)
  ())

(defun make-short-float-not-greater-instruction
    (input1 input2 successor1 successor2)
  (make-instance 'short-float-not-greater-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor1 successor2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SHORT-FLOAT-EQUAL-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SHORT-FLOAT.  It has no outputs.  It has two successors;
;;; the first one is chosen when the first input is equal to the
;;; second one, otherwise the second successor is chosen.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type.

(defclass short-float-equal-instruction (instruction two-successors-mixin)
  ())

(defun make-short-float-equal-instruction (input1 input2 successor1 successor2)
  (make-instance 'short-float-equal-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor1 successor2)))

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

(defclass short-float-sin-instruction (instruction one-successor-mixin)
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

(defclass short-float-cos-instruction (instruction one-successor-mixin)
  ())

(defun make-short-float-cos-instruction (input output successor)
  (make-instance 'short-float-cos-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SHORT-FLOAT-SQRT-INSTRUCTION.
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed SHORT-FLOAT.  It has a single output which is the
;;; unboxed SHORT-FLOAT square root of the input.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type.

(defclass short-float-sqrt-instruction (instruction one-successor-mixin)
  ())

(defun make-short-float-sqrt-instruction (input output successor)
  (make-instance 'short-float-sqrt-instruction
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

(defclass single-float-add-instruction (instruction one-successor-mixin)
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

(defclass single-float-sub-instruction (instruction one-successor-mixin)
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

(defclass single-float-mul-instruction (instruction one-successor-mixin)
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

(defclass single-float-div-instruction (instruction one-successor-mixin)
  ())

(defun make-single-float-div-instruction (input1 input2 output successor)
  (make-instance 'single-float-div-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SINGLE-FLOAT-LESS-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SINGLE-FLOAT.  It has no outputs.  It has two successors;
;;; the first one is chosen when the first input is strictly less than
;;; the second one, otherwise the second successor is chosen.
;;;
;;; This instruction can be used by implementations that support the
;;; SINGLE-FLOAT data type.

(defclass single-float-less-instruction (instruction two-successors-mixin)
  ())

(defun make-single-float-less-instruction (input1 input2 successor1 successor2)
  (make-instance 'single-float-less-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor1 successor2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SINGLE-FLOAT-NOT-GREATER-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SINGLE-FLOAT.  It has no outputs.  It has two successors;
;;; the first one is chosen when the first input is less than or equal
;;; to the second one, otherwise the second successor is chosen.
;;;
;;; This instruction can be used by implementations that support the
;;; SINGLE-FLOAT data type.

(defclass single-float-not-greater-instruction (instruction two-successors-mixin)
  ())

(defun make-single-float-not-greater-instruction
    (input1 input2 successor1 successor2)
  (make-instance 'single-float-not-greater-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor1 successor2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SINGLE-FLOAT-EQUAL-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed SINGLE-FLOAT.  It has no outputs.  It has two successors;
;;; the first one is chosen when the first input is equal to the
;;; second one, otherwise the second successor is chosen.
;;;
;;; This instruction can be used by implementations that support the
;;; SINGLE-FLOAT data type.

(defclass single-float-equal-instruction (instruction two-successors-mixin)
  ())

(defun make-single-float-equal-instruction (input1 input2 successor1 successor2)
  (make-instance 'single-float-equal-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor1 successor2)))

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

(defclass single-float-sin-instruction (instruction one-successor-mixin)
  ())

(defun make-single-float-sin-instruction (input output successor)
  (make-instance 'single-float-sin-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SINGLE-FLOAT-COS-INSTRUCTION.
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed SINGLE-FLOAT.  It has a single output which is the
;;; unboxed SINGLE-FLOAT cosine of the input.
;;;
;;; This instruction can be used by implementations that support the
;;; SINGLE-FLOAT data type.

(defclass single-float-cos-instruction (instruction one-successor-mixin)
  ())

(defun make-single-float-cos-instruction (input output successor)
  (make-instance 'single-float-cos-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION SINGLE-FLOAT-SQRT-INSTRUCTION.
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed SINGLE-FLOAT.  It has a single output which is the
;;; unboxed SINGLE-FLOAT square root of the input.
;;;
;;; This instruction can be used by implementations that support the
;;; SINGLE-FLOAT data type.

(defclass single-float-sqrt-instruction (instruction one-successor-mixin)
  ())

(defun make-single-float-sqrt-instruction (input output successor)
  (make-instance 'single-float-sqrt-instruction
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

(defclass double-float-add-instruction (instruction one-successor-mixin)
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

(defclass double-float-sub-instruction (instruction one-successor-mixin)
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

(defclass double-float-mul-instruction (instruction one-successor-mixin)
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

(defclass double-float-div-instruction (instruction one-successor-mixin)
  ())

(defun make-double-float-div-instruction (input1 input2 output successor)
  (make-instance 'double-float-div-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION DOUBLE-FLOAT-LESS-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed DOUBLE-FLOAT.  It has no outputs.  It has two successors;
;;; the first one is chosen when the first input is strictly less than
;;; the second one, otherwise the second successor is chosen.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type.

(defclass double-float-less-instruction (instruction two-successors-mixin)
  ())

(defun make-double-float-less-instruction (input1 input2 successor1 successor2)
  (make-instance 'double-float-less-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor1 successor2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION DOUBLE-FLOAT-NOT-GREATER-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed DOUBLE-FLOAT.  It has no outputs.  It has two successors;
;;; the first one is chosen when the first input is less than or equal
;;; to the second one, otherwise the second successor is chosen.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type.

(defclass double-float-not-greater-instruction (instruction two-successors-mixin)
  ())

(defun make-double-float-not-greater-instruction
    (input1 input2 successor1 successor2)
  (make-instance 'double-float-not-greater-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor1 successor2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION DOUBLE-FLOAT-EQUAL-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed DOUBLE-FLOAT.  It has no outputs.  It has two successors;
;;; the first one is chosen when the first input is equal to the
;;; second one, otherwise the second successor is chosen.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type.

(defclass double-float-equal-instruction (instruction two-successors-mixin)
  ())

(defun make-double-float-equal-instruction (input1 input2 successor1 successor2)
  (make-instance 'double-float-equal-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor1 successor2)))

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

(defclass double-float-sin-instruction (instruction one-successor-mixin)
  ())

(defun make-double-float-sin-instruction (input output successor)
  (make-instance 'double-float-sin-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION DOUBLE-FLOAT-COS-INSTRUCTION.
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed DOUBLE-FLOAT.  It has a single output which is the
;;; unboxed DOUBLE-FLOAT cosine of the input.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type.

(defclass double-float-cos-instruction (instruction one-successor-mixin)
  ())

(defun make-double-float-cos-instruction (input output successor)
  (make-instance 'double-float-cos-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION DOUBLE-FLOAT-SQRT-INSTRUCTION.
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed DOUBLE-FLOAT.  It has a single output which is the
;;; unboxed DOUBLE-FLOAT square root of the input.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type.

(defclass double-float-sqrt-instruction (instruction one-successor-mixin)
  ())

(defun make-double-float-sqrt-instruction (input output successor)
  (make-instance 'double-float-sqrt-instruction
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

(defclass long-float-add-instruction (instruction one-successor-mixin)
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

(defclass long-float-sub-instruction (instruction one-successor-mixin)
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

(defclass long-float-mul-instruction (instruction one-successor-mixin)
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

(defclass long-float-div-instruction (instruction one-successor-mixin)
  ())

(defun make-long-float-div-instruction (input1 input2 output successor)
  (make-instance 'long-float-div-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION LONG-FLOAT-LESS-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed LONG-FLOAT.  It has no outputs.  It has two successors;
;;; the first one is chosen when the first input is strictly less than
;;; the second one, otherwise the second successor is chosen.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type.

(defclass long-float-less-instruction (instruction two-successors-mixin)
  ())

(defun make-long-float-less-instruction (input1 input2 successor1 successor2)
  (make-instance 'long-float-less-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor1 successor2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION LONG-FLOAT-NOT-GREATER-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed LONG-FLOAT.  It has no outputs.  It has two successors;
;;; the first one is chosen when the first input is less than or equal
;;; to the second one, otherwise the second successor is chosen.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type.

(defclass long-float-not-greater-instruction (instruction two-successors-mixin)
  ())

(defun make-long-float-not-greater-instruction
    (input1 input2 successor1 successor2)
  (make-instance 'long-float-not-greater-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor1 successor2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION LONG-FLOAT-EQUAL-INSTRUCTION.
;;;
;;; This instruction takes two inputs which must be values of type
;;; unboxed LONG-FLOAT.  It has no outputs.  It has two successors;
;;; the first one is chosen when the first input is equal to the
;;; second one, otherwise the second successor is chosen.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type.

(defclass long-float-equal-instruction (instruction two-successors-mixin)
  ())

(defun make-long-float-equal-instruction (input1 input2 successor1 successor2)
  (make-instance 'long-float-equal-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor1 successor2)))

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

(defclass long-float-sin-instruction (instruction one-successor-mixin)
  ())

(defun make-long-float-sin-instruction (input output successor)
  (make-instance 'long-float-sin-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION LONG-FLOAT-COS-INSTRUCTION.
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed LONG-FLOAT.  It has a single output which is the
;;; unboxed LONG-FLOAT cosine of the input.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type.

(defclass long-float-cos-instruction (instruction one-successor-mixin)
  ())

(defun make-long-float-cos-instruction (input output successor)
  (make-instance 'long-float-cos-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION LONG-FLOAT-SQRT-INSTRUCTION.
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed LONG-FLOAT.  It has a single output which is the
;;; unboxed LONG-FLOAT square root of the input.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type.

(defclass long-float-sqrt-instruction (instruction one-successor-mixin)
  ())

(defun make-long-float-sqrt-instruction (input output successor)
  (make-instance 'long-float-sqrt-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION UNBOXED-INTEGER-TO-UNBOXED-SHORT-FLOAT-INSTRUCTION
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed integer, i.e.  It has a single output which is an
;;; unboxed SHORT-FLOAT presumably with the same value as the input.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type and for which the SHORT-FLOAT data type has
;;; a useful unboxed representation.

(defclass unboxed-integer-to-unboxed-short-float-instruction
    (instruction one-successor-mixin)
  ())

(defun make-unboxed-integer-to-unboxed-short-float-instruction
    (input output successor)
  (make-instance 'unboxed-integer-to-unboxed-short-float-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION UNBOXED-INTEGER-TO-UNBOXED-SINGLE-FLOAT-INSTRUCTION
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed integer, i.e.  It has a single output which is an
;;; unboxed SINGLE-FLOAT presumably with the same value as the input.
;;;
;;; This instruction can be used by implementations that support the
;;; SINGLE-FLOAT data type and for which the SINGLE-FLOAT data type
;;; has a useful unboxed representation.

(defclass unboxed-integer-to-unboxed-single-float-instruction
    (instruction one-successor-mixin)
  ())

(defun make-unboxed-integer-to-unboxed-single-float-instruction
    (input output successor)
  (make-instance 'unboxed-integer-to-unboxed-single-float-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION UNBOXED-INTEGER-TO-UNBOXED-DOUBLE-FLOAT-INSTRUCTION
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed integer, i.e.  It has a single output which is an
;;; unboxed DOUBLE-FLOAT presumably with the same value as the input.
;;;
;;; This instruction can be used by implementations that support the
;;; DOUBLE-FLOAT data type and for which the DOUBLE-FLOAT data type has
;;; a useful unboxed representation.

(defclass unboxed-integer-to-unboxed-double-float-instruction
    (instruction one-successor-mixin)
  ())

(defun make-unboxed-integer-to-unboxed-double-float-instruction
    (input output successor)
  (make-instance 'unboxed-integer-to-unboxed-double-float-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION UNBOXED-INTEGER-TO-UNBOXED-LONG-FLOAT-INSTRUCTION
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed integer, i.e.  It has a single output which is an
;;; unboxed LONG-FLOAT presumably with the same value as the input.
;;;
;;; This instruction can be used by implementations that support the
;;; LONG-FLOAT data type and for which the LONG-FLOAT data type has
;;; a useful unboxed representation.

(defclass unboxed-integer-to-unboxed-long-float-instruction
    (instruction one-successor-mixin)
  ())

(defun make-unboxed-integer-to-unboxed-long-float-instruction
    (input output successor)
  (make-instance 'unboxed-integer-to-unboxed-long-float-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION UNBOXED-SHORT-FLOAT-TO-UNBOXED-SINGLE-FLOAT-INSTRUCTION
;;;
;;; This instruction takes a single inputs which must be a value of
;;; type unboxed short-float, i.e.  It has a single output which is an
;;; unboxed SINGLE-FLOAT presumably with the same value as the input.
;;;
;;; This instruction can be used by implementations that support the
;;; SHORT-FLOAT data type and for which the SHORT-FLOAT and
;;; SINGLE-FLOAT data types have useful unboxed representations.

(defclass unboxed-short-float-to-unboxed-single-float-instruction
    (instruction one-successor-mixin)
  ())

(defun make-unboxed-short-float-to-unboxed-single-float-instruction
    (input output successor)
  (make-instance 'unboxed-short-float-to-unboxed-single-float-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))
