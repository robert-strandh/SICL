(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-T-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed
;;; to be a general array.  The second is assumed to be a FIXNUM
;;; and represents the index in the instance of the element to be read.
;;; This instruction produces a single output, the element read.

(defclass simple-t-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-simple-t-aref-instruction (input1 input2 output successor)
  (make-instance 'simple-t-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-T-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a general array.  The second is assumed to be a FIXNUM
;;; and represents the index in the instance of the element to be read.
;;; The third input is the element to be stored in the array.

(defclass simple-t-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-simple-t-aset-instruction (input1 input2 input3 successor)
  (make-instance 'simple-t-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction BIT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed
;;; to be an array specialized to BIT.  The second is assumed
;;; to be a FIXNUM and represents the index in the instance of the
;;; element to be read.  This instruction produces a single output,
;;; the element read, which is an unboxed BIT.

(defclass bit-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-bit-aref-instruction (input1 input2 output successor)
  (make-instance 'bit-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction BIT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be an array specialized to BIT.  The second is assumed
;;; to be a FIXNUM and represents the index in the instance of the
;;; element to be read.  The third input is assumed to be an unboxed
;;; BIT to be stored as an element in the array.

(defclass bit-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-bit-aset-instruction (input1 input2 input3 successor)
  (make-instance 'bit-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction UNSIGNED-BYTE-8-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be an array specialized to (UNSIGNED-BYTE 8).  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  This instruction produces a single
;;; output, the element read, which is an unboxed (UNSIGNED-BYTE 8).

(defclass unsigned-byte-8-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-unsigned-byte-8-aref-instruction (input1 input2 output successor)
  (make-instance 'unsigned-byte-8-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction UNSIGNED-BYTE-8-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be an array specialized to (UNSIGNED-BYTE 8).  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  The third input is assumed to be an
;;; unboxed (UNSIGNED-BYTE 8) to be stored as an element in the array.

(defclass unsigned-byte-8-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-unsigned-byte-8-aset-instruction (input1 input2 input3 successor)
  (make-instance 'unsigned-byte-8-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-SHORT-FLOAT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a simple array specialized to SHORT-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  This instruction produces a single
;;; output, the element read, which is an unboxed SHORT-FLOAT.

(defclass simple-short-float-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-simple-short-float-aref-instruction (input1 input2 output successor)
  (make-instance 'simple-short-float-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-SHORT-FLOAT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a simple array specialized to SHORT-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  The third input is assumed to be an
;;; unboxed SHORT-FLOAT to be stored as an element in the array.

(defclass simple-short-float-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-simple-short-float-aset-instruction (input1 input2 input3 successor)
  (make-instance 'simple-short-float-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SINGLE-FLOAT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed
;;; to be an array specialized to SINGLE-FLOAT.  The second is assumed
;;; to be a FIXNUM and represents the index in the instance of the
;;; element to be read.  This instruction produces a single output,
;;; the element read, which is an unboxed SINGLE-FLOAT.

(defclass single-float-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-single-float-aref-instruction (input1 input2 output successor)
  (make-instance 'single-float-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SINGLE-FLOAT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be an array specialized to SINGLE-FLOAT.  The second is assumed
;;; to be a FIXNUM and represents the index in the instance of the
;;; element to be read.  The third input is assumed to be an unboxed
;;; SINGLE-FLOAT to be stored as an element in the array.

(defclass single-float-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-single-float-aset-instruction (input1 input2 input3 successor)
  (make-instance 'single-float-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction DOUBLE-FLOAT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed
;;; to be an array specialized to DOUBLE-FLOAT.  The second is assumed
;;; to be a FIXNUM and represents the index in the instance of the
;;; element to be read.  This instruction produces a single output,
;;; the element read, which is an unboxed DOUBLE-FLOAT.

(defclass double-float-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-double-float-aref-instruction (input1 input2 output successor)
  (make-instance 'double-float-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction DOUBLE-FLOAT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be an array specialized to DOUBLE-FLOAT.  The second is assumed
;;; to be a FIXNUM and represents the index in the instance of the
;;; element to be read.  The third input is assumed to be an unboxed
;;; DOUBLE-FLOAT to be stored as an element in the array.

(defclass double-float-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-double-float-aset-instruction (input1 input2 input3 successor)
  (make-instance 'double-float-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction LONG-FLOAT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed
;;; to be an array specialized to LONG-FLOAT.  The second is assumed
;;; to be a FIXNUM and represents the index in the instance of the
;;; element to be read.  This instruction produces a single output,
;;; the element read, which is an unboxed LONG-FLOAT.

(defclass long-float-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-long-float-aref-instruction (input1 input2 output successor)
  (make-instance 'long-float-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction LONG-FLOAT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be an array specialized to LONG-FLOAT.  The second is assumed
;;; to be a FIXNUM and represents the index in the instance of the
;;; element to be read.  The third input is assumed to be an unboxed
;;; LONG-FLOAT to be stored as an element in the array.

(defclass long-float-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-long-float-aset-instruction (input1 input2 input3 successor)
  (make-instance 'long-float-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))
