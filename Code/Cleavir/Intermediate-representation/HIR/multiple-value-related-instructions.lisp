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

(defclass multiple-to-fixed-instruction (instruction one-successor-mixin)
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

(defclass fixed-to-multiple-instruction (instruction one-successor-mixin)
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
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defun make-multiple-value-call-instruction
    (inputs output &optional (successor nil successor-p))
  (make-instance 'multiple-value-call-instruction
    :inputs inputs
    :outputs (list output)
    :successors (if successor-p (list successor) '())))
