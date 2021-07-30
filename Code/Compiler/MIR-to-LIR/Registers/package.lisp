(cl:in-package #:cl-user)

(defpackage #:sicl-x86-64-registers
  (:use #:cl)
  (:export #:*rax* #:*rbx* #:*rcx* #:*rdx* #:*rsp* #:*rbp* #:*rsi* #:*rdi*
           #:*r8* #:*r9* #:*r10* #:*r11* #:*r12* #:*r13* #:*r14* #:*r15*
           #:*xmm0* #:*xmm1* #:*xmm2* #:*xmm3* #:*xmm4* #:*xmm5* #:*xmm6* #:*xmm7*
           #:*xmm8* #:*xmm9* #:*xmm10* #:*xmm11* #:*xmm12* #:*xmm13* #:*xmm14* #:*xmm15*
           #:*registers* #:*initial* #:+register-count+ #:*xmm* #:*gpr* 
           #:*dynamic-environment* #:*argument-count* #:*static-environment*
           #:*caller-saves* #:*callee-saves* #:*return-values*
           #:register-number-is-callee-saves-p
           #:*argument-registers* #:*return-registers*
           #:register-map
           #:register-number-in-map-p #:make-empty-register-map
           #:mark-register #:unmark-register
           #:copy-register-map #:reserve-register #:free-register
           #:register-map-difference #:register-map-intersection
           #:register-map-union #:find-any-register-in-map
           #:register-map-empty-p #:register-number #:make-register-map
           #:save-to-stack-instruction #:load-from-stack-instruction
           #:save-to-location-instruction #:load-from-location-instruction))
