(defpackage #:sicl-arm-assembler
  (:use #:common-lisp)
  (:shadow #:eq)
  (:export
   #:eq #:ne #:cs #:cc #:mi #:pl #:vs #:vc
   #:hi #:ls #:ge #:lt #:gt #:le #:||
   #:r0 #:r1 #:r2 #:r3 #:r4 #:r5 #:r6 #:r7 #:r8 #:r9 #:r10
   #:r11 #:r12 #:r13 #:r14 #:r15 #:sp #:lr #:pc
   #:lsl #:lsr #:asr #:ror
   ))

(defpackage #:sicl-arm-lir
  (:use #:common-lisp)
  (:shadow #:condition)
  (:export
   ))

