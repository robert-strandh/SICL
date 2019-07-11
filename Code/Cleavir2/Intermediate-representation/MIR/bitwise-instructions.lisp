(cl:in-package #:cleavir-ir)

(defclass binary-bitwise-instruction (instruction one-successor-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction binary-bitwise-instruction) slot-names
     &key
       inputs argument1 argument2
       outputs output
       successors successor)
  (assert (all-or-none argument1 argument2))
  (let ((inputs (combine inputs argument1 argument2))
        (outputs (if (null output) outputs (list output)))
        (successors (if (null successor) successors (list successor))))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

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

(defmethod shared-initialize :around
    ((instruction bitwise-not-instruction) slot-names
     &key
       inputs argument
       outputs output
       successors successor)
  (let ((inputs (if (null argument) inputs (list argument)))
        (outputs (if (null output) outputs (list output)))
        (successors (if (null successor) successors (list successor))))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

(defmethod argument ((instruction bitwise-not-instruction))
  (first (inputs instruction)))
