(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction MEMREF1-INSTRUCTION
;;;
;;; This instruction loads a memory location.  It takes a single input
;;; containing the address of the word to load.  It has a single
;;; output which is set to the contents of the memory location at the
;;; address.

(defclass memref1-instruction (instruction one-successor-mixin)
  ())

(normalize-arguments
 memref1-instruction
 (address)
 (output)
 (successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction MEMREF2-INSTRUCTION
;;;
;;; This instruction loads a memory location.  It takes two inputs.
;;; The first input is the base-address of a location in memory.  The
;;; second input is an offset to be added to the base address.  It has
;;; a single output which is set to the contents of the memory
;;; location at the address specified by the input and the offset
;;; added together.

(defclass memref2-instruction (instruction one-successor-mixin)
  ((%offset :initarg :offset :reader offset)))

(normalize-arguments
 memref2-instruction
 (base-address offset)
 (output)
 (successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction MEMSET1-INSTRUCTION
;;;
;;; This instruction stores an item in a memory location.  It takes
;;; two inputs.  The first input is the address of a location in
;;; memory.  The second input is the value to be stored in that
;;; location.

(defclass memset1-instruction (instruction one-successor-mixin)
  ())

(normalize-arguments
 memset1-instruction
 (address value)
 ()
 (successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction MEMSET2-INSTRUCTION
;;;
;;; This instruction stores an item in a memory location.  It takes
;;; three inputs.  The first input is the base-address of a location
;;; in memory.  The second input is an offset to be added to the base
;;; address.  The third input is the value to be stored in that
;;; location.  This instruction adds the input address to the offset
;;; and stores the input in the resulting address.

(defclass memset2-instruction (instruction one-successor-mixin)
  ((%offset :initarg :offset :reader offset)))

(normalize-arguments
 memset2-instruction
 (base-address offset value)
 ()
 (successor))
