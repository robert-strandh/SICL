(cl:in-package #:cleavir-ir)

(defclass binary-bitwise-instruction (instruction one-successor-mixin)
  ())

(normalize-arguments
 binary-bitwise-instruction
 (argument1 argument2)
 (output)
 (successor))

(defmethod argument1 ((instruction binary-bitwise-instruction))
  (first (inputs instruction)))

(defmethod argument2 ((instruction binary-bitwise-instruction))
  (second (inputs instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction BITWISE-AND-INSTRUCTION.

(defclass bitwise-and-instruction (binary-bitwise-instruction)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction BITWISE-OR-INSTRUCTION.

(defclass bitwise-or-instruction (binary-bitwise-instruction)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction BITWISE-EXCLUSIVE-OR-INSTRUCTION.

(defclass bitwise-exclusive-or-instruction (binary-bitwise-instruction)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction BITWISE-NOT-INSTRUCTION.

(defclass bitwise-not-instruction (instruction one-successor-mixin)
  ())

(normalize-arguments
 bitwise-not-instruction
 (argument)
 (output)
 (successor))
