(in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction MULTIPLE-TO-FIXED-INSTRUCTION.
;;;
;;; This instruction takes zero inputs and a list of outputs that are
;;; LEXICAL-LOCATIONs.  The purpose is to assign multiple values taken
;;; from the global values location to one or more ordinary lexical
;;; locations.  The assignment is done so that if fewer ordinary
;;; lexical locations are required than there are values contained in
;;; the global values location, then the remaining values are ignored,
;;; and if more ordinary lexical locations are required than there are
;;; values contained in the global values location, then the remaining
;;; ordinary lexical locations are assigned to NIL.

(defclass multiple-to-fixed-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FIXED-TO-MULTIPLE-INSTRUCTION.
;;;
;;; This instruction takes a list of inputs that are LEXICAL-LOCATIONs
;;; and a zero outputs.  The purpose is to assign one or more values,
;;; each contained in a LEXICAL-LOCATION to a the global values
;;; location.  The assignment is done so that all the inputs are
;;; preserved, and the number of inputs is kept with the global values
;;; location.

(defclass fixed-to-multiple-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction MULTIPLE-VALUE-CALL-INSTRUCTION.
;;;
;;; This instruction has a single input which is a lexical location
;;; holding a function to call.  The VALUES-ENVIRONMENT-LOCATION of
;;; this instruction holds any number of entries, each holding an
;;; arbitrary number of multiple values.  The top of the environment
;;; stack contains the values returned by the last sub-form of the
;;; MULTIPLE-VALUE-CALL form.  The values returned by the function
;;; call are stored in the global values location.

(defclass multiple-value-call-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction THE-VALUES-INSTRUCTION.
;;;
;;; This instruction is like THE-INSTRUCTION, but takes a multiple
;;; value inputs stored in the global values location, rather than the
;;; lexical locations that the THE-INSTRUCTION takes.  This
;;; instruction also has a (decomposed) values type instead of a
;;; single-value type.  A separate instruction is useful because the
;;; global values location can have an unknown or varying number of
;;; values.

(defclass the-values-instruction (instruction one-successor-mixin)
  ((%required-types :initarg :required :reader required-types)
   (%optional-types :initarg :optional :reader optional-types)
   (%rest-type :initarg :rest :reader rest-type)))

(defmethod clone-initargs append ((instruction the-values-instruction))
  (list :required (required-types instruction)
        :optional (optional-types instruction)
        :rest (rest-type instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SAVE-VALUES-INSTRUCTION.
;;;
;;; This instruction has a single output which is a lexical location
;;; holding a values environment.  The instruction takes the values in
;;; the global values location and creates a new values entry to be
;;; the top of the values environment stack.  The output holds the
;;; augmented values environment.

(defclass save-values-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction RESTORE-VALUES-INSTRUCTION.
;;;
;;; This instruction has no inputs and no outputs.  It takes the top
;;; entry of the values environment stored in the
;;; VALUES-ENVIRONMENT-LOCATION of this instruction, and stores that
;;; entry in the global values location.  The lexical location
;;; indicated in the VALUES-ENVIROMENT-LOCATION is then never used by
;;; any instruction executed after this one.

(defclass restore-values-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction INITIALIZE-VALUES-INSTRUCTION.
;;;
;;; This instruction has no inputs and a single output.  It is used in
;;; association with the MULTIPLE-VALUE-CALL-INSTRUCTION to initialize
;;; a suite of multiple values in a lexical location.  After the
;;; evaluation of each sub-form of the MULTIPLE-VALUE-CALL form, an
;;; APPEND-VALUES-INSTRUCTION takes the values of the sub-form and
;;; appends them to the accumulated values in the lexical output.

(defclass initialize-values-instruction (instruction one-successor-mixin)
  ())
