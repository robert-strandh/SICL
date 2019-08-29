(cl:in-package #:sicl-mir-to-lir)

;;;; In this version of SICL, we save all the arguments on the stack.
;;;; Recall that the five first arguments are passed in RDI, RSI, RDX,
;;;; RCX, and R8.  Remaining arguments are on top of the stack at
;;;; function entry, with the last argument at the highest address in
;;;; the frame, i.e. at RBP-16.
;;;;
;;;; To avoid too many tests for the exact number of arguments, we
;;;; always save the five registers.
;;;;
;;;; 1.  Subtract (5 + |lexicals|) * 8 from RSP.  The value to subract
;;;;     is a constant that is known at compile time.
;;;; 2.  Copy RSP to R11 (a scratch register)
;;;; 3.  Store RDI on the stack at the address indicated by R11.
;;;; 4.  Add 8 to R11.
;;;; 5.  Store RDI on the stack at the address indicated by R11.
;;;; 6.  Add 8 to R11.
;;;; 7.  Store RDI on the stack at the address indicated by R11.
;;;; 8 . Add 8 to R11.
;;;; 9.  Store RDI on the stack at the address indicated by R11.
;;;; 10. Add 8 to R11.
;;;; 11. Store RDI on the stack at the address indicated by R11.
;;;; 12. Add 8 to R11.
;;;; 13. Copy R11 to R12.
;;;; 14. Add |lexicals|*8 to R12.
;;;; 15. Copy RBP to R13.
;;;; 16. Subtract 8 from R13.
;;;; 17. While R12 < R13
;;;;   17.1 Load the contents of the stack at address R12 to R14.
;;;;   17.2 Store R15 to the stack at address R11.
;;;;   17.3 Add 8 to R11.
;;;;   17.2 Add 8 to R12.
;;;;
;;;; At this point, all the arguments are on top of the stack, in
;;;; order.  Between the arguments an the base pointer there is now
;;;; room for the lexical variables.
