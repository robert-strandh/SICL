(cl:in-package #:common-lisp-user)

(defpackage #:sicl-ir
  (:use #:common-lisp)
  (:export
   #:load-constant-instruction #:constants
   #:breakpoint-instruction
   #:named-call-mixin #:function-cell-cell
   #:named-call-instruction
   #:debug-information
   #:dynamic-environment-instruction
   #:caller-stack-pointer-instruction
   #:caller-frame-pointer-instruction
   #:establish-stack-frame-instruction
   #:push-instruction
   #:pop-instruction
   #:effective-address
   #:load-effective-address-instruction
   #:memref-effective-address-instruction
   #:memset-effective-address-instruction
   #:rack-instruction
   #:set-rack-instruction))
