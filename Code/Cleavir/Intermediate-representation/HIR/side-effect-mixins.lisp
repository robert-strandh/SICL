(in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class for instructions that perform allocation.
;;; (Not all instructions that could allocate, just useful ones)

(defclass allocation-mixin ()
  ;; whether the allocation can be done on the stack.
  ((%dynamic-extent-p :initform nil
     :initarg :dynamic-extent-p :accessor dynamic-extent-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class for instructions that have no side effects.

(defclass side-effect-free-mixin () ())

(defgeneric side-effect-free-p (instruction))

(defmethod side-effect-free-p (instruction)
  (declare (ignore instruction))
  nil)

(defmethod side-effect-free-p ((instruction side-effect-free-mixin))
  (declare (ignorable instruction))
  t)
