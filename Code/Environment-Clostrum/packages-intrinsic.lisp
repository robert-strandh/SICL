(cl:in-package #:common-lisp-user)

(defpackage #:sicl-environment
  (:use #:common-lisp)
  (:shadow
   .
   #1=(#:fdefinition
       #:find-class
       #:fboundp
       #:macro-function
       #:compiler-macro-function
       #:boundp
       #:symbol-value))
  (:local-nicknames
   (#:clo #:clostrum)
   (#:rt #:sicl-run-time))
  (:export
   #:*client*
   #:*environment*
   #:type-expander
   #:define-constant
   #:run-time-environment
   #:find-method-combination-template
   . #1#))
