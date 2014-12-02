(in-package #:cleavir-ir)

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
