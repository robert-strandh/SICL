(cl:in-package #:sicl-mir-to-lir)

;;;; When the function is entered, the return address is on top of the
;;;; stack.  We need to  move it to the address indicated by RBP-8.
;;;;
;;;; 1. Pop the stack into R11.
;;;; 2. Copy RBP to R12.
;;;; 3. Subtract 8 from R12.
;;;; 4. Store R11 to the stack at the address indicated by R12.
