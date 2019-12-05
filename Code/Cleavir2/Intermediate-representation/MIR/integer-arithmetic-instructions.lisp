(cl:in-package #:cleavir-ir)

(defmacro normalize-arguments
    (instruction-class-name input-names output-names successor-names)
  `(defmethod shared-initialize :around
     ((instruction ,instruction-class-name)
      slot-names
      &rest keys
      &key
      (inputs nil inputs-p) ,@input-names
      (outputs nil outputs-p) ,@output-names
      (successors nil successors-p) ,@successor-names)
     (declare (ignore inputs outputs successors))
     (cond ((or ,@input-names)
            (assert (and ,@input-names))
            (assert (not inputs-p))
            (cond ((or ,@output-names)
                   (assert (and ,@output-names))
                   (assert (not outputs-p))
                   (cond ((or ,@successor-names)
                          (assert (and ,@successor-names))
                          (assert (not successors-p))
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 :outputs (list ,@output-names)
                                 :successors (list ,@successor-names)
                                 keys))
                         (t
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 :outputs (list ,@output-names)
                                 keys))))
                  (t
                   (cond ((or ,@successor-names)
                          (assert (and ,@successor-names))
                          (assert (not successors-p))
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 :successors (list ,@successor-names)
                                 keys))
                         (t
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 keys))))))
           (t
            (cond ((or ,@output-names)
                   (assert (and ,@output-names))
                   (assert (not outputs-p))
                   (cond ((or ,@successor-names)
                          (assert (and ,@successor-names))
                          (assert (not successors-p))
                          (apply #'call-next-method instruction slot-names
                                 :outputs (list ,@output-names)
                                 :successors (list ,@successor-names)
                                 keys))
                         (t
                          (apply #'call-next-method instruction slot-names
                                 :outputs (list ,@output-names)
                                 keys))))
                  (t
                   (cond ((or ,@successor-names)
                          (assert (and ,@successor-names))
                          (assert (not successors-p))
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 :successors (list ,@successor-names)
                                 keys))
                         (t
                          (apply #'call-next-method instruction slot-names
                                 keys)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-ADD-INSTRUCTION

(defclass signed-add-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 signed-add-instruction
 (augend addend)
 (output)
 (normal-successor overflow-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-SUB-INSTRUCTION

(defclass signed-sub-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 signed-sub-instruction
 (minuend subtrahend)
 (output)
 (normal-successor overflow-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-LESS-INSTRUCTION

(defclass signed-less-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 signed-less-instruction
 (argument1 argument2)
 (output)
 (true-successor false-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-NOT-GREATER-INSTRUCTION

(defclass signed-not-greater-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 signed-not-greater-instruction
 (argument1 argument2)
 (output)
 (true-successor false-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-ADD-INSTRUCTION

(defclass unsigned-add-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 unsigned-add-instruction
 (augend addend)
 (output)
 (normal-successor carry-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-SUB-INSTRUCTION

(defclass unsigned-sub-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 unsigned-sub-instruction
 (minuend subtrahend)
 (output)
 (normal-successor carry-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-DIV-INSTRUCTION

(defclass unsigned-div-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 unsigned-div-instruction
 (dividend divisor)
 (quotient remainder)
 (successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-LESS-INSTRUCTION

(defclass unsigned-less-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 unsigned-less-instruction
 (argument1 argument2)
 (output)
 (true-successor false-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-NOT-GREATER-INSTRUCTION

(defclass unsigned-not-greater-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 unsigned-not-greater-instruction
 (argument1 argument2)
 (output)
 (true-successor false-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction EQUAL-INSTRUCTION

(defclass equal-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 equal-instruction
 (argument1 argument2)
 (output)
 (true-successor false-successor))
