(in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction MULTIPLE-TO-FIXED-INSTRUCTION.
;;;
;;; This instruction takes a single input of type VALUES-LOCATION and
;;; a list of outputs that are LEXICAL-LOCATIONs.  The purpose is to
;;; assign multiple values to one or more ordinary lexical locations.
;;; The assignment is done so that if fewer ordinary lexical locations
;;; are required than there are values contained in the
;;; VALUES-LOCATION, then the remaining values are ignored, and if
;;; more ordinary lexical locations are required than there are values
;;; contained in the VALUES-LOCATION, then the remaining ordinary
;;; lexical locations are assigned to NIL.

(defclass multiple-to-fixed-instruction (one-successor-mixin instruction)
  ())

(defun make-multiple-to-fixed-instruction
    (input outputs &optional (successor nil successor-p))
  (make-instance 'multiple-to-fixed-instruction
    :inputs (list input)
    :outputs outputs
    :successors (if successor-p (list successor) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FIXED-TO-MULTIPLE-INSTRUCTION.
;;;
;;; This instruction takes a list of inputs that are LEXICAL-LOCATIONs
;;; and a single output of type VALUES-LOCATION.  The purpose is to
;;; assign one or more values, each contained in a LEXICAL-LOCATION to
;;; a location containing multiple values.  The assignment is done so
;;; that all the inputs are preserved, and the number of inputs is
;;; kept with the output.

(defclass fixed-to-multiple-instruction (one-successor-mixin instruction)
  ())

(defun make-fixed-to-multiple-instruction
    (inputs output &optional (successor nil successor-p))
  (make-instance 'fixed-to-multiple-instruction
    :inputs inputs
    :outputs (list output)
    :successors (if successor-p (list successor) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction MULTIPLE-VALUE-CALL-INSTRUCTION.
;;;
;;; The first input of this instruction is an ordinary lexical
;;; location.  The remaining inputs are of type VALUES-LOCATION, and
;;; each represents multiple values returned form the evaluation of
;;; some form.  This instruction has a single output, also of the type
;;; VALUES-LOCATION.

(defclass multiple-value-call-instruction
    (one-successor-mixin side-effect-mixin instruction)
  ;; default KLUDGE
  ((%attributes :initarg :attributes :initform 0 :reader attributes)))

(defun make-multiple-value-call-instruction
    (inputs output &optional (successor nil successor-p) (attributes 0))
  (make-instance 'multiple-value-call-instruction
    :inputs inputs
    :outputs (list output)
    :successors (if successor-p (list successor) '())
    :attributes attributes))

(defmethod clone-initargs append
    ((instruction multiple-value-call-instruction))
  (list :attributes (attributes instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction THE-VALUES-INSTRUCTION.
;;;
;;; This is like THE-INSTRUCTION, but takes a VALUES-LOCATION
;;; as input instead of a lexical one, and correspondingly, a
;;; (decomposed) values type instead of a single-value type.
;;; A separate instruction is useful because values locations can
;;; have an unknown or varying number of values.

(defclass the-values-instruction (one-successor-mixin instruction)
  ((%required-types :initarg :required :reader required-types)
   (%optional-types :initarg :optional :reader optional-types)
   (%rest-type :initarg :rest :reader rest-type)))

(defun make-the-values-instruction (input successor
				    required optional rest)
  (make-instance 'the-values-instruction
    :inputs (list input)
    :outputs '()
    :successors (list successor)
    :required required
    :optional optional
    :rest rest))

(defmethod clone-initargs append ((instruction the-values-instruction))
  (list :required (required-types instruction)
        :optional (optional-types instruction)
        :rest (rest-type instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SAVE-VALUES-INSTRUCTION.
;;;
;;; This instruction has a single output which is a lexical location
;;; holding a the dynamic environment.  The instruction takes the
;;; values in the global values location and creates a new values
;;; entry to be the top of the dynamic environment.  The output holds
;;; the augmented dynamic environment.

;;; FIXME: this instruction should not be a subclass of
;;; SIDE-EFFECT-MIXIN.  It is temporarily the case until we fix
;;; REMOVE-USELESS-INSTRUCTIONS to take dynamic environment locations
;;; into account.  Right now, the output of this instruction is
;;; considered not used, so the instruction gets removed.

(defclass save-values-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction LOAD-VALUES-INSTRUCTION.
;;;
;;; This instruction has no inputs. It reads the values entry on the
;;; top of the dynamic environment, and fills the global values
;;; location with those values.
;;;
;;; Note that this instruction only reads from the dynamic
;;; environment, and does not "pop" the top entry. This should be
;;; done by a LOCAL-UNWIND-INSTRUCTION instead. These concerns are
;;; separate so that code can unwind without restoring the values,
;;; e.g. in (block nil (multiple-value-prog1 form1 (return form2))).

(defclass load-values-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())
