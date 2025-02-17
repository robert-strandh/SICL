(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hir
  (:use #:common-lisp)
  (:export
   #:instruction
   #:inputs
   #:outputs
   #:successors
   #:parse-arguments-instruction
   #:make-cell-instruction
   #:read-cell-instruction
   #:write-cell-instruction
   #:set-static-environment-instruction
   #:static-environment-instruction
   #:dynamic-environment-instruction
   #:read-static-environment-instruction
   #:if-instruction
   #:exit-point-instruction
   #:unwind-instruction
   #:receive-instruction
   #:funcall-instruction
   #:return-instruction
   #:assignment-instruction
   #:special-variable-binding-instruction
   #:enclose-instruction
   #:datum
   #:register
   #:readers
   #:writers
   #:value
   #:single-value-register
   #:multiple-value-register
   #:literal))
