(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-ADD-INSTRUCTION

(defclass signed-add-instruction (instruction multiple-successors-mixin)
  ())

(defun make-signed-add-instruction
    (&key
       ((:input1 i1) nil i1-p)
       ((:input2 i2) nil i2-p)
       ((:inputs i) nil i-p)
       ((:output o) nil o-p)
       ((:successor1 s1) nil s1-p)
       ((:successor2 s2) nil s2-p)
       ((:successors s) nil s-p))
  (make-instance 'signed-add-instruction
    :inputs (construct-inputs i i-p i1 i1-p i2 i2-p)
    :outputs (construct-output o o-p)
    :successors (construct-successors s s-p s1 s1-p s2 s2-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-SUB-INSTRUCTION

(defclass signed-sub-instruction (instruction multiple-successors-mixin)
  ())

(defun make-signed-sub-instruction
    (&key
       ((:input1 i1) nil i1-p)
       ((:input2 i2) nil i2-p)
       ((:inputs i) nil i-p)
       ((:output o) nil o-p)
       ((:successor1 s1) nil s1-p)
       ((:successor2 s2) nil s2-p)
       ((:successors s) nil s-p))
  (make-instance 'signed-sub-instruction
    :inputs (construct-inputs i i-p i1 i1-p i2 i2-p)
    :outputs (construct-output o o-p)
    :successors (construct-successors s s-p s1 s1-p s2 s2-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-LESS-INSTRUCTION

(defclass signed-less-instruction (instruction multiple-successors-mixin)
  ())

(defun make-signed-less-instruction
    (&key
       ((:input1 i1) nil i1-p)
       ((:input2 i2) nil i2-p)
       ((:inputs i) nil i-p)
       ((:successor1 s1) nil s1-p)
       ((:successor2 s2) nil s2-p)
       ((:successors s) nil s-p))
  (make-instance 'signed-less-instruction
    :inputs (construct-inputs i i-p i1 i1-p i2 i2-p)
    :successors (construct-successors s s-p s1 s1-p s2 s2-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-NOT-GREATER-INSTRUCTION

(defclass signed-not-greater-instruction (instruction multiple-successors-mixin)
  ())

(defun make-signed-not-greater-instruction
    (&key
       ((:input1 i1) nil i1-p)
       ((:input2 i2) nil i2-p)
       ((:inputs i) nil i-p)
       ((:successor1 s1) nil s1-p)
       ((:successor2 s2) nil s2-p)
       ((:successors s) nil s-p))
  (make-instance 'signed-not-greater-instruction
    :inputs (construct-inputs i i-p i1 i1-p i2 i2-p)
    :successors (construct-successors s s-p s1 s1-p s2 s2-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-ADD-INSTRUCTION

(defclass unsigned-add-instruction (instruction multiple-successors-mixin)
  ())

(defun make-unsigned-add-instruction
    (&key
       ((:input1 i1) nil i1-p)
       ((:input2 i2) nil i2-p)
       ((:inputs i) nil i-p)
       ((:output o) nil o-p)
       ((:successor1 s1) nil s1-p)
       ((:successor2 s2) nil s2-p)
       ((:successors s) nil s-p))
  (make-instance 'unsigned-add-instruction
    :inputs (construct-inputs i i-p i1 i1-p i2 i2-p)
    :outputs (construct-output o o-p)
    :successors (construct-successors s s-p s1 s1-p s2 s2-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-SUB-INSTRUCTION

(defclass unsigned-sub-instruction (instruction multiple-successors-mixin)
  ())

(defun make-unsigned-sub-instruction
    (&key
       ((:input1 i1) nil i1-p)
       ((:input2 i2) nil i2-p)
       ((:inputs i) nil i-p)
       ((:output o) nil o-p)
       ((:successor1 s1) nil s1-p)
       ((:successor2 s2) nil s2-p)
       ((:successors s) nil s-p))
  (make-instance 'unsigned-sub-instruction
    :inputs (construct-inputs i i-p i1 i1-p i2 i2-p)
    :outputs (construct-output o o-p)
    :successors (construct-successors s s-p s1 s1-p s2 s2-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-LESS-INSTRUCTION

(defclass unsigned-less-instruction (instruction multiple-successors-mixin)
  ())

(defun make-unsigned-less-instruction
    (&key
       ((:input1 i1) nil i1-p)
       ((:input2 i2) nil i2-p)
       ((:inputs i) nil i-p)
       ((:successor1 s1) nil s1-p)
       ((:successor2 s2) nil s2-p)
       ((:successors s) nil s-p))
  (make-instance 'unsigned-less-instruction
    :inputs (construct-inputs i i-p i1 i1-p i2 i2-p)
    :successors (construct-successors s s-p s1 s1-p s2 s2-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-NOT-GREATER-INSTRUCTION

(defclass unsigned-not-greater-instruction (instruction multiple-successors-mixin)
  ())

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
