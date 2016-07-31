(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFINE-ARRAY-INSTRUCTIONS.
;;;
;;; This macro defines four instructions and their associated
;;; constructors: one instruction for reading an element of a simple
;;; array, one instruction for writing an element of a simple array,
;;; one instruction for reading an element of a non-simple array, one
;;; instruction for writing an element of a non-simple array,
(defmacro define-array-instructions
    (simple-aref-instruction
     simple-aref-constructor
     simple-aset-instruction
     simple-aset-constructor
     non-simple-aref-instruction
     non-simple-aref-constructor
     non-simple-aset-instruction
     non-simple-aset-constructor)
  `(progn
     (defclass ,simple-aref-instruction
	 (instruction one-successor-mixin)
       ())

     (defun ,simple-aref-constructor (input1 input2 output successor)
       (make-instance ',simple-aref-instruction
	 :inputs (list input1 input2)
	 :outputs (list output)
	 :successors (list successor)))

     (defclass ,simple-aset-instruction
	 (instruction one-successor-mixin side-effect-mixin)
       ())

     (defun ,simple-aset-constructor (input1 input2 input3 successor)
       (make-instance ',simple-aset-instruction
	 :inputs (list input1 input2 input3)
	 :outputs ()
	 :successors (list successor)))

     (defclass ,non-simple-aref-instruction
	 (instruction one-successor-mixin)
       ())

     (defun ,non-simple-aref-constructor (input1 input2 output successor)
       (make-instance ',non-simple-aref-instruction
	 :inputs (list input1 input2)
	 :outputs (list output)
	 :successors (list successor)))

     (defclass ,non-simple-aset-instruction
	 (instruction one-successor-mixin side-effect-mixin)
       ())

     (defun ,non-simple-aset-constructor (input1 input2 input3 successor)
       (make-instance ',non-simple-aset-instruction
	 :inputs (list input1 input2 input3)
	 :outputs ()
	 :successors (list successor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-T-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be an unspecialized simple array.  The second is assumed to be a
;;; FIXNUM and represents the index in the instance of the element to
;;; be read.  This instruction produces a single output, the element
;;; read.

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
;;; to be an unspecialized simple array.  The second is assumed to be
;;; a FIXNUM and represents the index in the instance of the element
;;; to be read.  The third input is the element to be stored in the
;;; array.

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
;;; Instruction NON-SIMPLE-T-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be an unspecialized non-simple array.  The second is assumed to be
;;; a FIXNUM and represents the index in the instance of the element
;;; to be read.  This instruction produces a single output, the
;;; element read.

(defclass non-simple-t-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-non-simple-t-aref-instruction (input1 input2 output successor)
  (make-instance 'non-simple-t-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NON-SIMPLE-T-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be an unspecialized non-simple array.  The second is assumed to
;;; be a FIXNUM and represents the index in the instance of the
;;; element to be read.  The third input is the element to be stored
;;; in the array.

(defclass non-simple-t-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-non-simple-t-aset-instruction (input1 input2 input3 successor)
  (make-instance 'non-simple-t-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-BIT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a simple array specialized to BIT.  The second is assumed to be
;;; a FIXNUM and represents the index in the instance of the element
;;; to be read.  This instruction produces a single output, the
;;; element read, which is an unboxed BIT.

(defclass simple-bit-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-simple-bit-aref-instruction (input1 input2 output successor)
  (make-instance 'simple-bit-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-BIT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a simple array specialized to BIT.  The second is assumed to
;;; be a FIXNUM and represents the index in the instance of the
;;; element to be read.  The third input is assumed to be an unboxed
;;; BIT to be stored as an element in the array.

(defclass simple-bit-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-simple-bit-aset-instruction (input1 input2 input3 successor)
  (make-instance 'simple-bit-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NON-SIMPLE-BIT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a non-simple array specialized to BIT.  The second is assumed to be
;;; a FIXNUM and represents the index in the instance of the element
;;; to be read.  This instruction produces a single output, the
;;; element read, which is an unboxed BIT.

(defclass non-simple-bit-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-non-simple-bit-aref-instruction (input1 input2 output successor)
  (make-instance 'non-simple-bit-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NON-SIMPLE-BIT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a non-simple array specialized to BIT.  The second is assumed to
;;; be a FIXNUM and represents the index in the instance of the
;;; element to be read.  The third input is assumed to be an unboxed
;;; BIT to be stored as an element in the array.

(defclass non-simple-bit-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-non-simple-bit-aset-instruction (input1 input2 input3 successor)
  (make-instance 'non-simple-bit-aset-instruction
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
;;; Instruction NON-SIMPLE-SHORT-FLOAT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a non-simple array specialized to SHORT-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  This instruction produces a single
;;; output, the element read, which is an unboxed SHORT-FLOAT.

(defclass non-simple-short-float-aref-instruction
    (instruction one-successor-mixin)
  ())

(defun make-non-simple-short-float-aref-instruction
    (input1 input2 output successor)
  (make-instance 'non-simple-short-float-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NON-SIMPLE-SHORT-FLOAT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a non-simple array specialized to SHORT-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  The third input is assumed to be an
;;; unboxed SHORT-FLOAT to be stored as an element in the array.

(defclass non-simple-short-float-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-non-simple-short-float-aset-instruction
    (input1 input2 input3 successor)
  (make-instance 'non-simple-short-float-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-SINGLE-FLOAT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a simple array specialized to SINGLE-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  This instruction produces a single
;;; output, the element read, which is an unboxed SINGLE-FLOAT.

(defclass simple-single-float-aref-instruction
    (instruction one-successor-mixin)
  ())

(defun make-simple-single-float-aref-instruction
    (input1 input2 output successor)
  (make-instance 'simple-single-float-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-SINGLE-FLOAT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a simple array specialized to SINGLE-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  The third input is assumed to be an
;;; unboxed SINGLE-FLOAT to be stored as an element in the array.

(defclass simple-single-float-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-simple-single-float-aset-instruction
    (input1 input2 input3 successor)
  (make-instance 'simple-single-float-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NON-SIMPLE-SINGLE-FLOAT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a non-simple array specialized to SINGLE-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  This instruction produces a single
;;; output, the element read, which is an unboxed SINGLE-FLOAT.

(defclass non-simple-single-float-aref-instruction
    (instruction one-successor-mixin)
  ())

(defun make-non-simple-single-float-aref-instruction
    (input1 input2 output successor)
  (make-instance 'non-simple-single-float-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NON-SIMPLE-SINGLE-FLOAT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a non-simple array specialized to SINGLE-FLOAT.  The second
;;; is assumed to be a FIXNUM and represents the index in the instance
;;; of the element to be read.  The third input is assumed to be an
;;; unboxed SINGLE-FLOAT to be stored as an element in the array.

(defclass non-simple-single-float-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-non-simple-single-float-aset-instruction
    (input1 input2 input3 successor)
  (make-instance 'non-simple-single-float-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-DOUBLE-FLOAT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a simple array specialized to DOUBLE-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  This instruction produces a single
;;; output, the element read, which is an unboxed DOUBLE-FLOAT.

(defclass simple-double-float-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-simple-double-float-aref-instruction (input1 input2 output successor)
  (make-instance 'simple-double-float-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-DOUBLE-FLOAT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a simple array specialized to DOUBLE-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  The third input is assumed to be an
;;; unboxed DOUBLE-FLOAT to be stored as an element in the array.

(defclass simple-double-float-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-simple-double-float-aset-instruction (input1 input2 input3 successor)
  (make-instance 'simple-double-float-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NON-SIMPLE-DOUBLE-FLOAT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a non-simple array specialized to DOUBLE-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  This instruction produces a single
;;; output, the element read, which is an unboxed DOUBLE-FLOAT.

(defclass non-simple-double-float-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-non-simple-double-float-aref-instruction
    (input1 input2 output successor)
  (make-instance 'non-simple-double-float-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NON-SIMPLE-DOUBLE-FLOAT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a non-simple array specialized to DOUBLE-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  The third input is assumed to be an
;;; unboxed DOUBLE-FLOAT to be stored as an element in the array.

(defclass non-simple-double-float-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-non-simple-double-float-aset-instruction
    (input1 input2 input3 successor)
  (make-instance 'non-simple-double-float-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-LONG-FLOAT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a simple array specialized to LONG-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  This instruction produces a single
;;; output, the element read, which is an unboxed LONG-FLOAT.

(defclass simple-long-float-aref-instruction (instruction one-successor-mixin)
  ())

(defun make-simple-long-float-aref-instruction (input1 input2 output successor)
  (make-instance 'simple-long-float-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SIMPLE-LONG-FLOAT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a simple array specialized to LONG-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  The third input is assumed to be an
;;; unboxed LONG-FLOAT to be stored as an element in the array.

(defclass simple-long-float-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-simple-long-float-aset-instruction (input1 input2 input3 successor)
  (make-instance 'simple-long-float-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NON-SIMPLE-LONG-FLOAT-AREF-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a non-simple array specialized to LONG-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  This instruction produces a single
;;; output, the element read, which is an unboxed LONG-FLOAT.

(defclass non-simple-long-float-aref-instruction
    (instruction one-successor-mixin)
  ())

(defun make-non-simple-long-float-aref-instruction
    (input1 input2 output successor)
  (make-instance 'non-simple-long-float-aref-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NON-SIMPLE-LONG-FLOAT-ASET-INSTRUCTION.
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a non-simple array specialized to LONG-FLOAT.  The second is
;;; assumed to be a FIXNUM and represents the index in the instance of
;;; the element to be read.  The third input is assumed to be an
;;; unboxed LONG-FLOAT to be stored as an element in the array.

(defclass non-simple-long-float-aset-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-non-simple-long-float-aset-instruction
    (input1 input2 input3 successor)
  (make-instance 'non-simple-long-float-aset-instruction
    :inputs (list input1 input2 input3)
    :outputs ()
    :successors (list successor)))
