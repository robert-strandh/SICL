(cl:in-package #:common-lisp-user)

(defpackage #:sicl-environment
  (:use #:common-lisp)
  (:local-nicknames
   (#:clo #:clostrum)
   (#:rt #:sicl-run-time))
  (:shadow #:get-setf-expansion)
  (:export
   #:*client*
   #:*environment*
   #:type-expander
   #:define-constant
   #:run-time-environment
   #:find-method-combination-template
   #:get-setf-expansion))
