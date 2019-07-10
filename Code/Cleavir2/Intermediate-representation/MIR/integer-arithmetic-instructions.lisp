(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-ADD-INSTRUCTION

(defclass signed-add-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction signed-add-instruction) slot-names
     &key
       inputs augend addend
       outputs output
       successors normal-successor overflow-successor)
  (assert (both-or-none augend addend))
  (assert (both-or-none normal-successor overflow-successor))
  (let ((inputs (combine inputs augend addend))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors normal-successor overflow-successor)))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-SUB-INSTRUCTION

(defclass signed-sub-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction signed-sub-instruction) slot-names
     &key
       inputs minuend subtrahend
       outputs output
       successors normal-successor overflow-successor)
  (assert (both-or-none minuend subtrahend))
  (assert (both-or-none normal-successor overflow-successor))
  (let ((inputs (combine inputs minuend subtrahend))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors normal-successor overflow-successor)))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-LESS-INSTRUCTION

(defclass signed-less-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction signed-less-instruction) slot-names
     &key
       inputs argument1 argument2
       outputs output
       successors true-successor false-successor)
  (assert (both-or-none argument1 argument2))
  (assert (both-or-none true-successor false-successor))
  (let ((inputs (combine inputs argument1 argument2))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors true-successor false-successor)))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-NOT-GREATER-INSTRUCTION

(defclass signed-not-greater-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction signed-not-greater-instruction) slot-names
     &key
       inputs argument1 argument2
       outputs output
       successors true-successor false-successor)
  (assert (both-or-none argument1 argument2))
  (assert (both-or-none true-successor false-successor))
  (let ((inputs (combine inputs argument1 argument2))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors true-successor false-successor)))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-ADD-INSTRUCTION

(defclass unsigned-add-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction unsigned-add-instruction) slot-names
     &key
       inputs augend addend
       outputs output
       successors normal-successor carry-successor)
  (assert (both-or-none augend addend))
  (assert (both-or-none normal-successor carry-successor))
  (let ((inputs (combine inputs augend addend))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors normal-successor carry-successor)))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-SUB-INSTRUCTION

(defclass unsigned-sub-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction unsigned-sub-instruction) slot-names
     &key
       inputs minuend subtrahend
       outputs output
       successors normal-successor carry-successor)
  (assert (both-or-none minuend subtrahend))
  (assert (both-or-none normal-successor carry-successor))
  (let ((inputs (combine inputs minuend subtrahend))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors normal-successor carry-successor)))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-LESS-INSTRUCTION

(defclass unsigned-less-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction unsigned-less-instruction) slot-names
     &key
       inputs argument1 argument2
       outputs output
       successors true-successor false-successor)
  (assert (both-or-none argument1 argument2))
  (assert (both-or-none true-successor false-successor))
  (let ((inputs (combine inputs argument1 argument2))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors true-successor false-successor)))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-NOT-GREATER-INSTRUCTION

(defclass unsigned-not-greater-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction unsigned-not-greater-instruction) slot-names
     &key
       inputs argument1 argument2
       outputs output
       successors true-successor false-successor)
  (assert (both-or-none argument1 argument2))
  (assert (both-or-none true-successor false-successor))
  (let ((inputs (combine inputs argument1 argument2))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors true-successor false-successor)))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction EQUAL-INSTRUCTION

(defclass equal-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction equal-instruction) slot-names
     &key
       inputs argument1 argument2
       outputs output
       successors true-successor false-successor)
  (assert (both-or-none argument1 argument2))
  (assert (both-or-none true-successor false-successor))
  (let ((inputs (combine inputs argument1 argument2))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors true-successor false-successor)))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))
