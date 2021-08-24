(cl:in-package #:sicl-ir)

;;; This instruction computes an effective address into a
;;; register.  (x86 documentation still uses the verb "load" to
;;; describe the action performed, so we follow suit.)
;;; 
;;; It has four inputs: a base register, an index register, an
;;; immediate scale (which can be one of 1, 2, 4 or 8), and an
;;; immediate displacement. It has a single output, and a single
;;; successor.
(defclass load-effective-address-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

;;; This instruction loads from an effective address.
;;;
;;; The inputs of this instruction are the same as for
;;; LOAD-EFFECTIVE-ADDRESS-INSTRUCTION, but this instruction also has
;;; a single output. This instruction has a single successor.
(defclass memref-effective-address-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

;;; This instruction stores to an effective address.  The inputs of
;;; this instruction are the same as for
;;; LOAD-EFFECTIVE-ADDRESS-INSTRUCTION, with the addition of a fifth
;;; input with the value to store. This instruction has no outputs,
;;; and a single successor.
(defclass memset-effective-address-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

;;; A datum which represents an argument in an effective address which
;;; should not be used.  While there are MEMREF2 and MEMSET2
;;; instructions that do not use the index functionality, there is no
;;; equivalent to LOAD-EFFECTIVE-ADDRESS-INSTRUCTION.
(defclass nowhere (cleavir-ir:datum)
  ())

(defun nowhere ()
  (make-instance 'nowhere))
