(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction INITIALIZE-CLOSURE-INSTRUCTION.
;;;
;;; This instruction takes as the first input, a lexical location
;;; holding the output of an ENCLOSE-INSTRUCTION.  The rest of the
;;; inputs initialize the closure with their values.
(defclass initialize-closure-instruction (instruction one-successor-mixin
                                          side-effect-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CREATE-CELL-INSTRUCTION.
;;;
;;; This instruction has no inputs and a single output.  The output is
;;; a lexical location that will hold the cell being created by the
;;; instruction.

(defclass create-cell-instruction (instruction one-successor-mixin
                                   allocation-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FETCH-INSTRUCTION.
;;;
;;; This instruction is used when the static environment is
;;; represented as a single vector containing an entry for each
;;; closed-over variable.  Such an entry is either a CELL (with
;;; unspecified representation) if the corresponding variable is
;;; modified, or the entry can be the variable itself if it is not
;;; modified.
;;;
;;; This instruction takes two inputs.  The first input is a dynamic
;;; lexical location that holds the static environment.  The second
;;; input is an immediate input containing a non-negative integer and
;;; which serves as an index into the static environment.  This
;;; instruction has a single output, which is a dynamic lexical
;;; location.

(defclass fetch-instruction (instruction one-successor-mixin)
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction READ-CELL-INSTRUCTION.
;;;
;;; Since the structure of a CELL holding the value of a closed-over
;;; variable is unspecified, we use this instruction to obtain the
;;; value held in such a CELL.
;;;
;;; This instruction takes a single input, namely a dynamic lexical
;;; location holding the cell to be read from.  This instruction has a
;;; single output, namely a dynamic lexical location to hold the value
;;; of the variable.

(defclass read-cell-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction WRITE-CELL-INSTRUCTION.
;;;
;;; Since the structure of a CELL holding the value of a closed-over
;;; variable is unspecified, we use this instruction to write the
;;; value held in such a CELL.
;;;
;;; This instruction takes two inputs. The first input is a dynamic
;;; lexical location holding the cell to be written to.  The second
;;; input is a constant input or a dynamic lexical input holding the
;;; value to write to the cell.  This instruction has no outputs.

(defclass write-cell-instruction (instruction one-successor-mixin side-effect-mixin)
  ())

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
