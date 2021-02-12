(cl:in-package #:common-lisp-user)

(defpackage #:sicl-ir
  (:use #:common-lisp)
  (:export
   #:breakpoint-instruction
   #:named-call-mixin #:function-cell-cell
   #:named-call-instruction
   #:debug-information
   #:dynamic-environment-instruction
   #:caller-stack-pointer-instruction
   #:caller-frame-pointer-instruction
   #:establish-stack-frame-instruction
   #:push-instruction
   #:pop-instruction))
