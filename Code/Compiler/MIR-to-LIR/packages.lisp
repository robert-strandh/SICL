(cl:in-package #:common-lisp-user)

(defpackage #:sicl-mir-to-lir
  (:use #:common-lisp)
  (:local-nicknames (#:x86-64 #:sicl-x86-64-registers))
  (:export
   #:mir-to-lir
   #:*rax*
   #:*rax*
   #:*rbx*
   #:*rcx*
   #:*rdx*
   #:*rsp*
   #:*rbp*
   #:*rsi*
   #:*rdi*
   #:*r8*
   #:*r9*
   #:*r10*
   #:*r11*
   #:*r12*
   #:*r13*
   #:*r14*
   #:*r15*))
