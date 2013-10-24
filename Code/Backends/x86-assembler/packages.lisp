(cl:in-package #:common-lisp-user)

(defpackage #:x86-assembler
  (:use #:common-lisp)
  (:export
   #:make-label
   #:make-code-command
   #:make-immediate-operand
   #:make-gpr-operand
   #:make-memory-operand
   #:assemble
   ))

