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
  (assert (or (and (null augend) (null addend))
              (and (not (null augend)) (not (null addend)))))
  (assert (or (and (null normal-successor) (null overflow-successor))
              (and (not (null normal-successor)) (not (null overflow-successor)))))
  (let ((inputs (if (and (null augend) (null addend))
                    inputs
                    (list augend addend)))
        (outputs (if (null output)
                     outputs
                     (list output)))
        (successors (if (and (null normal-successor) (null overflow-successor))
                        successors
                        (list normal-successor overflow-successor))))
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
  (assert (or (and (null minuend) (null subtrahend))
              (and (not (null minuend)) (not (null subtrahend)))))
  (assert (or (and (null normal-successor) (null overflow-successor))
              (and (not (null normal-successor)) (not (null overflow-successor)))))
  (let ((inputs (if (and (null minuend) (null subtrahend))
                    inputs
                    (list minuend subtrahend)))
        (outputs (if (null output)
                     outputs
                     (list output)))
        (successors (if (and (null normal-successor) (null overflow-successor))
                        successors
                        (list normal-successor overflow-successor))))
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
  (assert (or (and (null argument1) (null argument2))
              (and (not (null argument1)) (not (null argument2)))))
  (assert (or (and (null true-successor) (null false-successor))
              (and (not (null true-successor)) (not (null false-successor)))))
  (let ((inputs (if (and (null argument1) (null argument2))
                    inputs
                    (list argument1 argument2)))
        (outputs (if (null output)
                     outputs
                     (list output)))
        (successors (if (and (null true-successor) (null false-successor))
                        successors
                        (list true-successor false-successor))))
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
  (assert (or (and (null argument1) (null argument2))
              (and (not (null argument1)) (not (null argument2)))))
  (assert (or (and (null true-successor) (null false-successor))
              (and (not (null true-successor)) (not (null false-successor)))))
  (let ((inputs (if (and (null argument1) (null argument2))
                    inputs
                    (list argument1 argument2)))
        (outputs (if (null output)
                     outputs
                     (list output)))
        (successors (if (and (null true-successor) (null false-successor))
                        successors
                        (list true-successor false-successor))))
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
  (assert (or (and (null augend) (null addend))
              (and (not (null augend)) (not (null addend)))))
  (assert (or (and (null normal-successor) (null carry-successor))
              (and (not (null normal-successor)) (not (null carry-successor)))))
  (let ((inputs (if (and (null augend) (null addend))
                    inputs
                    (list augend addend)))
        (outputs (if (null output)
                     outputs
                     (list output)))
        (successors (if (and (null normal-successor) (null carry-successor))
                        successors
                        (list normal-successor carry-successor))))
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
  (assert (or (and (null minuend) (null subtrahend))
              (and (not (null minuend)) (not (null subtrahend)))))
  (assert (or (and (null normal-successor) (null carry-successor))
              (and (not (null normal-successor)) (not (null carry-successor)))))
  (let ((inputs (if (and (null minuend) (null subtrahend))
                    inputs
                    (list minuend subtrahend)))
        (outputs (if (null output)
                     outputs
                     (list output)))
        (successors (if (and (null normal-successor) (null carry-successor))
                        successors
                        (list normal-successor carry-successor))))
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
  (assert (or (and (null argument1) (null argument2))
              (and (not (null argument1)) (not (null argument2)))))
  (assert (or (and (null true-successor) (null false-successor))
              (and (not (null true-successor)) (not (null false-successor)))))
  (let ((inputs (if (and (null argument1) (null argument2))
                    inputs
                    (list argument1 argument2)))
        (outputs (if (null output)
                     outputs
                     (list output)))
        (successors (if (and (null true-successor) (null false-successor))
                        successors
                        (list true-successor false-successor))))
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
  (assert (or (and (null argument1) (null argument2))
              (and (not (null argument1)) (not (null argument2)))))
  (assert (or (and (null true-successor) (null false-successor))
              (and (not (null true-successor)) (not (null false-successor)))))
  (let ((inputs (if (and (null argument1) (null argument2))
                    inputs
                    (list argument1 argument2)))
        (outputs (if (null output)
                     outputs
                     (list output)))
        (successors (if (and (null true-successor) (null false-successor))
                        successors
                        (list true-successor false-successor))))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

(defun make-unsigned-not-greater-instruction
    (&key
       ((:input1 i1) nil i1-p)
       ((:input2 i2) nil i2-p)
       ((:inputs i) nil i-p)
       ((:successor1 s1) nil s1-p)
       ((:successor2 s2) nil s2-p)
       ((:successors s) nil s-p))
  (make-instance 'unsigned-not-greater-instruction
    :inputs (construct-inputs i i-p i1 i1-p i2 i2-p)
    :successors (construct-successors s s-p s1 s1-p s2 s2-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction EQUAL-INSTRUCTION

(defclass equal-instruction (instruction multiple-successors-mixin)
  ())

(defun make-equal-instruction
    (&key
       ((:input1 i1) nil i1-p)
       ((:input2 i2) nil i2-p)
       ((:inputs i) nil i-p)
       ((:successor1 s1) nil s1-p)
       ((:successor2 s2) nil s2-p)
       ((:successors s) nil s-p))
  (make-instance 'equal-instruction
    :inputs (construct-inputs i i-p i1 i1-p i2 i2-p)
    :successors (construct-successors s s-p s1 s1-p s2 s2-p)))
