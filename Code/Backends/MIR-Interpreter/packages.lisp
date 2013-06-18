(cl:defpackage #:sicl-mir-interpreter
  (:use #:common-lisp)
  (:shadow #:function #:eval)
  (:export
   #:eval
   ))
