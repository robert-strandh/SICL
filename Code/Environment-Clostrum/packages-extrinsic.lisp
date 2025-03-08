(cl:in-package #:common-lisp-user)

(defpackage #:sicl-environment
  (:use #:common-lisp)
  (:local-nicknames
   (#:clo #:clostrum)
   #+(and)(#:rt #:common-boot-hir-evaluator)
   #+(or)(#:rt #:common-boot-ast-interpreter))
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
          #:symbol-value
          #:get-setf-expansion))
  (:export
   #:*client*
   #:*environment*
   #:type-expander
   #:define-constant
   #:run-time-environment
   #:find-method-combination-template
   . #1#))
