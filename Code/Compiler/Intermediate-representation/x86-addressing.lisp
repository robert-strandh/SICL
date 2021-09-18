(cl:in-package #:sicl-ir)

;;; A datum representing an effective address on the x86 and x86-64
;;; processors.  An effective address is the linear expression
;;;     base + offset * constant + displacement
;;; of a base register, offset register, constant scale, and constant
;;; displacement.  Furthermore, the scale must be one of 0, 1, 2, 4 or
;;; 8.

(defclass effective-address (cleavir-ir:datum)
  ((base   :initarg :base   :reader base)
   (offset :initarg :offset :reader offset)
   (scale  :initarg :scale :reader scale)
   (displacement :initarg :displacement :reader displacement)))

(defun effective-address (base &key offset (scale 0) (displacement 0))
  (make-instance 'effective-address
                 :base base
                 :offset offset
                 :scale scale
                 :displacement displacement))

(defmethod print-object ((address effective-address) stream)
  (print-unreadable-object (address stream :type t)
    (write-string (cleavir-ir:name (base address)) stream)
    (unless (zerop (scale address))
      (format stream " + ~D * ~A"
              (scale address)
              (cleavir-ir:name (offset address))))
    (unless (zerop (displacement address))
      (format stream " + ~D" (displacement address)))))

;;; This instruction computes an effective address into a
;;; register.  (x86 documentation still uses the verb "load" to
;;; describe the action performed, so we follow suit.)
;;; 
;;; It has one input, which is the effective address to compute.
(defclass load-effective-address-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

;;; This instruction loads from an effective address.
;;;
;;; It has one input, one output and one successor.
(defclass memref-effective-address-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

;;; This instruction stores to an effective address.
;;;
;;; It has two inputs, the effective address to store to and the value
;;; to store, no outputs, and one successor.
(defclass memset-effective-address-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())
