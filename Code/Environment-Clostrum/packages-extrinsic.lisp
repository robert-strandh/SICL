(cl:in-package #:common-lisp-user)

(defpackage #:sicl-environment
  (:use #:common-lisp)
  (:local-nicknames (#:clo #:clostrum))
  (:shadow
   . #1= (#:symbol-function
          #:fdefinition
          #:find-class
          #:find-package
          #:fboundp
          #:fmakunbound
          #:macro-function))
  (:export
   . #1#))
