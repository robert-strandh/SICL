(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION BOX-INSTRUCTION.
;;;
;;; This instruction takes one input, which must be an unboxed
;;; value of the ELEMENT-TYPE of the instruction. It has one output
;;; which is that value, boxed.

(defclass box-instruction (instruction one-successor-mixin)
  ((%element-type :initarg :element-type :accessor element-type)))

(defmethod clone-instruction :around ((instruction box-instruction))
  (let ((result (call-next-method)))
    (setf (element-type result) (element-type instruction))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSTRUCTION UNBOX-INSTRUCTION.
;;;
;;; This instruction takes one input, a boxed (normal) value of
;;; type ELEMENT-TYPE. It has one output, the value unboxed.

(defclass unbox-instruction (instruction one-successor-mixin)
  ((%element-type :initarg :element-type :accessor element-type)))

(defmethod clone-instruction :around ((instruction unbox-instruction))
  (let ((result (call-next-method)))
    (setf (element-type result) (element-type instruction))
    result))
