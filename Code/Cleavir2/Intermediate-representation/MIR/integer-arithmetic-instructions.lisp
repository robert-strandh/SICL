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

(defmethod shared-initialize :around
    ((instruction signed-add-instruction) slot-names
     &rest keys
     &key
       inputs augend addend
       outputs output
       successors normal-successor overflow-successor)
  (assert (all-or-none augend addend))
  (assert (all-or-none normal-successor overflow-successor))
  (let ((inputs (combine inputs augend addend))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors normal-successor overflow-successor)))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-SUB-INSTRUCTION

(defclass signed-sub-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction signed-sub-instruction) slot-names
     &rest keys
     &key
       inputs minuend subtrahend
       outputs output
       successors normal-successor overflow-successor)
  (assert (all-or-none minuend subtrahend))
  (assert (all-or-none normal-successor overflow-successor))
  (let ((inputs (combine inputs minuend subtrahend))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors normal-successor overflow-successor)))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-LESS-INSTRUCTION

(defclass signed-less-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction signed-less-instruction) slot-names
     &rest keys
     &key
       inputs argument1 argument2
       outputs output
       successors true-successor false-successor)
  (assert (all-or-none argument1 argument2))
  (assert (all-or-none true-successor false-successor))
  (let ((inputs (combine inputs argument1 argument2))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors true-successor false-successor)))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-NOT-GREATER-INSTRUCTION

(defclass signed-not-greater-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction signed-not-greater-instruction) slot-names
     &rest keys
     &key
       inputs argument1 argument2
       outputs output
       successors true-successor false-successor)
  (assert (all-or-none argument1 argument2))
  (assert (all-or-none true-successor false-successor))
  (let ((inputs (combine inputs argument1 argument2))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors true-successor false-successor)))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))

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

(defmethod shared-initialize :around
    ((instruction unsigned-sub-instruction) slot-names
     &rest keys
     &key
       inputs minuend subtrahend
       outputs output
       successors normal-successor carry-successor)
  (assert (all-or-none minuend subtrahend))
  (assert (all-or-none normal-successor carry-successor))
  (let ((inputs (combine inputs minuend subtrahend))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors normal-successor carry-successor)))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-DIV-INSTRUCTION

(defclass unsigned-div-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction unsigned-div-instruction) slot-names
     &rest keys
     &key
       inputs dividend divisor
       outputs quotient remainder
       successors successor)
  (assert (all-or-none dividend divisor))
  (let ((inputs (combine inputs dividend divisor))
        (outputs (combine outputs quotient remainder))
        (successors (if (null successor) successors (list successor))))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-LESS-INSTRUCTION

(defclass unsigned-less-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction unsigned-less-instruction) slot-names
     &rest keys
     &key
       inputs argument1 argument2
       outputs output
       successors true-successor false-successor)
  (assert (all-or-none argument1 argument2))
  (assert (all-or-none true-successor false-successor))
  (let ((inputs (combine inputs argument1 argument2))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors true-successor false-successor)))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-NOT-GREATER-INSTRUCTION

(defclass unsigned-not-greater-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction unsigned-not-greater-instruction) slot-names
     &rest keys
     &key
       inputs argument1 argument2
       outputs output
       successors true-successor false-successor)
  (assert (all-or-none argument1 argument2))
  (assert (all-or-none true-successor false-successor))
  (let ((inputs (combine inputs argument1 argument2))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors true-successor false-successor)))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction EQUAL-INSTRUCTION

(defclass equal-instruction (instruction multiple-successors-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction equal-instruction) slot-names
     &rest keys
     &key
       inputs argument1 argument2
       outputs output
       successors true-successor false-successor)
  (assert (all-or-none argument1 argument2))
  (assert (all-or-none true-successor false-successor))
  (let ((inputs (combine inputs argument1 argument2))
        (outputs (if (null output) outputs (list output)))
        (successors (combine successors true-successor false-successor)))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))
