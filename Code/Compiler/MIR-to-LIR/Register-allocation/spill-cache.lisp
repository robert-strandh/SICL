(cl:in-package #:sicl-register-allocation)

;;; By SPILL, we mean to copy a particular register R to any free
;;; stack slot.  Since a register can be attributed to several lexical
;;; locations, in a particular arrangement, there can be several
;;; attributions with the same information about register and stack
;;; slot.  For that reason, we must find every attribution with R in
;;; it and provide modified attributions in the new arrangement.  The
;;; spill is made explicit in the MIR code as an
;;; ASSIGNMENT-INSTRUCTION I with the same lexical location in the
;;; input and the output.  The input arrangement of I contains one or
;;; more attributions with R in them and with a NIL stack slot.  The
;;; output arrangement of I is similar to the input arrangement, but
;;; with the attributions containing R having a valid stack slot
;;; number in them.

;;; By CACHE, we mean to copy a particular stack slot to a register.
;;; As with the SPILL, several attributions may be involved.  As with
;;; SPILL, the CACHE is made explicit with an ASSIGNMENT-INSTRUCTION
;;; with the analogous input and output arrangements.
