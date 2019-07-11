(cl:in-package #:cleavir-ir)

(defclass bitwise-instruction (instruction one-successor-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction bitwise-instruction) slot-names
     &key
       inputs argument1 argument2
       outputs output
       successors successor)
  (assert (both-or-none argument1 argument2))
  (let ((inputs (combine inputs argument1 argument2))
        (outputs (if (null output) outputs (list output)))
        (successors (if (null successor) successors (list successor))))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

(defmethod argument1 ((instruction bitwise-instruction))
  (first (inputs instruction)))

(defmethod argument2 ((instruction bitwise-instruction))
  (second (inputs instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction BITWISE-AND-INSTRUCTION.

(defclass bitwise-and-instruction (bitwise-instruction)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction BITWISE-OR-INSTRUCTION.

(defclass bitwise-or-instruction (bitwise-instruction)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction BITWISE-EXCLUSIVE-OR-INSTRUCTION.

(defclass bitwise-exclusive-or-instruction (bitwise-instruction)
  ())
