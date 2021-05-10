(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin classes for instructions according to successor count.

;;; Mixin class for instructions with no successors. 
(defclass no-successors-mixin () ())

;;; Mixin class for instructions with a single successor.
(defclass one-successor-mixin () ())

;;; Mixin class for instructions with more than one successor.
(defclass multiple-successors-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin classes for instructions that implement some operation such
;;; as z <- x op y.

;;; Mixin class for all such instructions.
(defclass binary-operation-mixin () ())

;;; Mixin class for commutative such instructions.
(defclass commutative-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class for comparison instructions, i.e., instructions that
;;; take two inputs, has no output, and has two successors.
(defclass comparison-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class for test instructions, i.e., instructions that
;;; take one input, has no output, and has two successors.
(defclass test-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Other mixin classes.

;;; Mixin class for instructions that have side effects so that they
;;; should not be removed.
(defclass side-effect-mixin () ())

;;; Helper function.

(defgeneric side-effect-free-p (instruction))
(defmethod side-effect-free-p (instruction) t)
(defmethod side-effect-free-p ((instruction side-effect-mixin)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class for instructions that perform allocation.
;;; (Not all instructions that could allocate, just useful ones)

(defclass allocation-mixin ()
  ;; whether the allocation can be done on the stack.
  ((%dynamic-extent-p :initform nil
     :initarg :dynamic-extent-p :accessor dynamic-extent-p)))
