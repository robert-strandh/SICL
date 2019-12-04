(cl:in-package #:cleavir-ir)

(defgeneric shifted-input (instruction))

(defgeneric shift-count (instruction))

(defclass shift-instruction (instruction one-successor-mixin)
  ())

(defmethod shifted-input ((instruction shift-instruction))
  (first (inputs instruction)))

(defmethod shift-count ((instruction shift-instruction))
  (second (inputs instruction)))

(defmethod shared-initialize :around
    ((instruction shift-instruction) slot-names
     &rest keys
     &key
       inputs shifted-input shift-count
       outputs output
       successors successor)
  (assert (all-or-none shifted-input shift-count))
  (let ((inputs (combine inputs shifted-input shift-count))
        (outputs (if (null output) outputs (list output)))
        (successors (if (null successor) successors (list successors))))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SHIFT-LEFT-INSTRUCTION.

(defclass shift-left-instruction (shift-instruction)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction LOGIC-SHIFT-RIGHT-INSTRUCTION.

(defclass logic-shift-right-instruction (shift-instruction)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction ARITHMETIC-SHIFT-RIGHT-INSTRUCTION.

(defclass arithmetic-shift-right-instruction (shift-instruction)
  ())
