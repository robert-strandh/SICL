(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction MEMREF1-INSTRUCTION
;;;
;;; This instruction loads a memory location.  It takes a single input
;;; containing the address of the word to load.  It has a single
;;; output which is set to the contents of the memory location at the
;;; address.

(defclass memref1-instruction (instruction one-successor-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction memref1-instruction) slot-names
     &key
       inputs address
       outputs output
       successors successor)
  (let ((inputs (if (null address) inputs (list address)))
        (outputs (if (null output) outputs (list output)))
        (successors (if (null successor) successors (list successor))))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))

(defun make-memref1-instruction (address output &optional successor)
  (make-instance 'memref1-instruction
    :inputs (list address)
    :outputs (list output)
    :successors (if (null successor) '() (list successor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction MEMREF2-INSTRUCTION
;;;
;;; This instruction loads a memory location.  It takes a single input
;;; containing the address of the word to load.  It has a single
;;; output which is set to the contents of the memory location at the
;;; address specified by the input and the offset added together.

(defclass memref2-instruction (instruction one-successor-mixin)
  ((%offset :initarg :offset :reader offset)))

(defun make-memref2-instruction (base-address offset output &optional successor)
  (make-instance 'memref2-instruction
    :inputs (list base-address)
    :offset offset
    :outputs (list output)
    :successors (if (null successor) '() (list successor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction MEMSET1-INSTRUCTION
;;;
;;; This instruction stores an item in a memory location.  It takes
;;; two inputs.  The first input is the address of a location in
;;; memory.  The second input is the item to be stored in that
;;; location.

(defclass memset1-instruction (instruction one-successor-mixin)
  ())

(defun make-memset1-instruction (address value &optional successor)
  (make-instance 'memset1-instruction
    :inputs (list address value)
    :outputs '()
    :successors (if (null successor) '() (list successor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction MEMSET2-INSTRUCTION
;;;
;;; This instruction stores an item in a memory location.  It takes
;;; two inputs.  The first input is the address of a location in
;;; memory.  The second input is the item to be stored in that
;;; location.  This instruction adds the input address to the offset
;;; and stores the input in the resulting address.

(defclass memset2-instruction (instruction one-successor-mixin)
  ((%offset :initarg :offset :reader offset)))

(defun make-memset2-instruction (base-address offset value &optional successor)
  (make-instance 'memset2-instruction
    :inputs (list base-address value)
    :offset offset
    :outputs '()
    :successors (if (null successor) '() (list successor))))
