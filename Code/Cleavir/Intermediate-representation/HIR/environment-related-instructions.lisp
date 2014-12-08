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
;;; second input is the size of the activation record to add.  This
;;; instruction has a single output, which is a dynamic lexical
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
