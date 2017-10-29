(cl:in-package #:cleavir-partial-inlining)

;;; The first input to CALL-INSTRUCTION is a temporary that is the
;;; output of ENCLOSE-INSTRUCTION, and ENCLOSE-INSTRUCTION has
;;; ENTER-INSTRUCTION as its input.
;;;
;;; Furthermore, we know that no other call-instruction has the same
;;; first input as CALL-INSTRUCTION, so we are free to mutate
;;; CALL-INSTRUCTION and ENTER-INSTRUCTION to add more inputs and
;;; outputs respectively.
;;;
;;; MAPPING is a mapping that takes an intruction or a datum in the
;;; callee and maps it to an instruction or a datum in the caller,
;;; provided such a correspondance has been added to the mapping.
(defgeneric inline-line-instruction
  (enclose-instruction call-instruction enter-instruction mapping))
