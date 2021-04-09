(cl:in-package #:sicl-register-allocation)

;;; In the output arrangement of PREDECESSOR, LEXICAL-LOCATION has a
;;; register attributed to it, but no stack slot attributed to it.  We
;;; insert an ASSIGNMENT-INSTRUCTION A between PREDECESSOR and
;;; INSTRUCTION that has LEXICAL-LOCATION both as its input and its
;;; output.  The input arrangement of A is the same as the output
;;; arrangement of PREDECESSOR.  The output arrangement of A is
;;; similar to the input arrangement, except that it has a stack slot
;;; attributed to LEXICAL-LOCATION, in addition to the register that
;;; is attributed to it in the input arrangement of A.
(defun spill (predecessor instruction lexical-location)
  (let* ((arrangement (output-arrangement predecessor))
         (new-arrangement (arr:copy-arrangement arrangement))
         (new-instruction (make-instance 'cleavir-ir:assignment-instruction
                            :input lexical-location
                            :output lexical-location)))
    (arr:attribute-stack-slot new-arrangement lexical-location)
    (setf (input-arrangement new-instruction) arrangement
          (output-arrangement new-instruction) new-arrangement)
    (cleavir-ir:insert-instruction-between
     new-instruction predecessor instruction)
    new-instruction))

;;; In the output arrangement of PREDECESSOR, LEXICAL-LOCATION has
;;; both a register and a stack slot attributed to it.  We insert a
;;; NOP-INSTRUCTION N between PREDECESSOR and INSTRUCTION.  The input
;;; arrangement of N is the same as the output arrangement of
;;; PREDECESSOR.  The output arrangement of N is similar to the input
;;; arrangement, except that it has no register attributed to
;;; LEXICAL-LOCATION.
(defun unattribute-register (predecessor instruction lexical-location)
  (let* ((arrangement (output-arrangement predecessor))
         (new-arrangement (arr:copy-arrangement arrangement))
         (new-instruction (make-instance 'cleavir-ir:nop-instruction
                            :input lexical-location
                            :output lexical-location)))
    (arr:unattribute-register new-arrangement lexical-location)
    (setf (input-arrangement new-instruction) arrangement
          (output-arrangement new-instruction) new-arrangement)
    (cleavir-ir:insert-instruction-between
     new-instruction predecessor instruction)
    new-instruction))

(defun spill-and-unattribute-register (predecessor instruction register-number)
  (unattribute-register
   (spill predecessor instruction register-number)
   instruction
   register-number))

;;; In the output arrangement of PREDECESSOR, LEXICAL-LOCATION has a
;;; stack slot attributed to it, but no register attributed to it.  We
;;; insert an ASSIGNMENT-INSTRUCTION A between PREDECESSOR and
;;; INSTRUCTION that has LEXICAL-LOCATION both as its input and its
;;; output.  The input arrangement of A is the same as the output
;;; arrangement of PREDECESSOR.  The output arrangement of A is
;;; similar to the input arrangement, except that it has a register
;;; attributed to LEXICAL-LOCATION, in addition to the stack that
;;; is attributed to it in the input arrangement of A.
(defun unspill (predecessor instruction lexical-location candidates)
  (let* ((arrangement (output-arrangement predecessor))
         (new-arrangement (arr:copy-arrangement arrangement))
         (new-instruction (make-instance 'cleavir-ir:assignment-instruction
                            :input lexical-location
                            :output lexical-location)))
    (arr:attribute-register-for-existing-lexical-location
     new-arrangement lexical-location candidates)
    (setf (input-arrangement new-instruction) arrangement
          (output-arrangement new-instruction) new-arrangement)
    (cleavir-ir:insert-instruction-between
     new-instruction predecessor instruction)
    new-instruction))
