(cl:in-package #:common-lisp-user)

(defpackage #:sicl-environment
  (:use #:common-lisp)
  (:local-nicknames
   (#:clo #:clostrum)
   (#:rt #:common-boot-ast-evaluator))
  (:shadow
   . #1= (#:symbol-function
          #:fdefinition
          #:find-class
          #:find-package
          #:fboundp
          #:fmakunbound
          #:macro-function
          #:compiler-macro-function
          #:boundp
          #:symbol-value))
  (:export
   #:define-constant
   . #1#))
