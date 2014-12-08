(in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ADD-ACTIVATION-RECORD-INSTRUCTION.
;;;
;;; The purpose of this instruction is to add an activation record on
;;; top of an existing static runtime environment.
;;;
;;; This instruction takes two inputs.  The first input is a dynamic
;;; lexical location that holds a static runtime environment.  The
;;; second input is either an immediate input or a dynamic lexical
;;; location that holds the size of the activation record to add.
;;; This instruction has a single output, which is a dynamic lexical
;;; location that holds the new static runtime environment that has
;;; the additional activation record on it.  Notice that the input
;;; dynamic lexical location remains valid and is subject to register
;;; allocation.

(defclass add-activation-record-instruction (instruction one-successor-mixin)
  ())

(defun make-add-activation-record-instruction (env-input size-input env-output)
  (make-instance 'add-activation-record-instruction
    :inputs (list env-input size-input)
    :outputs (list env-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction REMOVE-ACTIVATION-RECORD-INSTRUCTION.
;;;
;;; The purpose of this instruction is to remove an activation record
;;; from the top of an existing static runtime environment.
;;;
;;; This instruction takes a single input, a dynamic lexical location
;;; that holds a static runtime environment.  This instruction has a
;;; single output, which is a dynamic lexical location that holds the
;;; new static runtime environment that is like the original one,
;;; except that the top activation record has been removed.  Notice
;;; that the input dynamic lexical location remains valid and is
;;; subject to register allocation.

(defclass remove-activation-record-instruction (instruction one-successor-mixin)
  ())

(defun make-remove-activation-record-instruction (env-input env-output)
  (make-instance 'remove-activation-record-instruction
    :inputs (list env-input)
    :outputs (list env-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction LOAD-FROM-STATIC-ENVIRONMENT-INSTRUCTION
;;;
;;; The purpose of this instruction is to access a value stored in the
;;; static runtime environment and copy it into a dynamic lexical
;;; location.
;;;
;;; This instruction takes two inputs.  The first input is a dynamic
;;; lexical location that holds a static runtime environment.  The
;;; second input is either an immediate input or a dynamic lexical
;;; location with a non-negative integer value that indicates what
;;; variable in the topmost activation record should be loaded.  The
;;; output is a dynamic lexical location.

(defclass load-from-static-environment-instruction
    (instruction one-successor-mixin)
  ())

(defun make-load-from-static-environment-instruction
    (env-input offset-input output)
  (make-instance 'load-from-static-environment-instruction
    :inputs (list env-input offset-input)
    :outputs (list output)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction STORE-TO-STATIC-ENVIRONMENT-INSTRUCTION
;;;
;;; The purpose of this instruction is to copy a value from a dynamic
;;; lexical location or a constant input to the static runtime
;;; environment.
;;;
;;; This instruction takes three inputs.  The first input is a dynamic
;;; lexical location that holds a static runtime environment.  The
;;; second input is either an immediate input or a dynamic lexical
;;; location with a non-negative integer value that indicates what
;;; variable in the topmost activation record should be stored to.
;;; The third input is a dynamic lexical location or a constant input
;;; to be copied.

(defclass store-to-static-environment-instruction
    (instruction one-successor-mixin)
  ())

(defun make-store-to-static-environment-instruction
    (env-input offset-input value-input)
  (make-instance 'store-to-static-environment-instruction
    :inputs (list env-input offset-input value-input)
    :outputs '()))
