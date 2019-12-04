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
     &rest keys
     &key
       inputs address
       outputs output
       successors successor)
  (let ((inputs (if (null address) inputs (list address)))
        (outputs (if (null output) outputs (list output)))
        (successors (if (null successor) successors (list successor))))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction MEMREF2-INSTRUCTION
;;;
;;; This instruction loads a memory location.  It takes two inputs.
;;; The first input is the base-address of a location in memory.  The
;;; second input is an offset to be added to the base address.  It has
;;; a single output which is set to the contents of the memory
;;; location at the address specified by the input and the offset
;;; added together.

(defclass memref2-instruction (instruction one-successor-mixin)
  ((%offset :initarg :offset :reader offset)))

(defmethod shared-initialize :around
    ((instruction memref2-instruction) slot-names
     &rest keys
     &key
       inputs base-address offset
       outputs output
       successors successor)
  (assert (all-or-none base-address offset))
  (let ((inputs (combine inputs base-address offset))
        (outputs (if (null output) outputs (list output)))
        (successors (if (null successor) successors (list successor))))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction MEMSET1-INSTRUCTION
;;;
;;; This instruction stores an item in a memory location.  It takes
;;; two inputs.  The first input is the address of a location in
;;; memory.  The second input is the value to be stored in that
;;; location.

(defclass memset1-instruction (instruction one-successor-mixin)
  ())


(defmethod shared-initialize :around
    ((instruction memset1-instruction) slot-names
     &rest keys
     &key
       inputs address value
       outputs output
       successors successor)
  (assert (all-or-none address value))
  (assert (null outputs))
  (assert (null output))
  (let ((inputs (combine inputs address value))
        (successors (if (null successor) successors (list successor))))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction MEMSET2-INSTRUCTION
;;;
;;; This instruction stores an item in a memory location.  It takes
;;; three inputs.  The first input is the base-address of a location
;;; in memory.  The second input is an offset to be added to the base
;;; address.  The third input is the value to be stored in that
;;; location.  This instruction adds the input address to the offset
;;; and stores the input in the resulting address.

(defclass memset2-instruction (instruction one-successor-mixin)
  ((%offset :initarg :offset :reader offset)))

(defmethod shared-initialize :around
    ((instruction memset2-instruction) slot-names
     &rest keys
     &key
       inputs base-address offset value
       outputs output
       successors successor)
  (assert (all-or-none base-address offset value))
  (assert (null outputs))
  (assert (null output))
  (let ((inputs (combine inputs base-address offset value))
        (successors (if (null successor) successors (list successor))))
    (apply #'call-next-method instruction slot-names
           :inputs inputs
           :outputs outputs
           :successors successors
           keys)))
