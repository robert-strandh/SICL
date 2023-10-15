(cl:in-package #:common-lisp-user)

(defpackage #:sicl-new-boot
  (:use #:common-lisp)
  (:local-nicknames
   (#:abp #:architecture.builder-protocol)
   (#:cmd #:common-macro-definitions)
   (#:cb #:common-boot)
   (#:cbae #:common-boot-ast-evaluator))
  (:export
   #:ensure-asdf-system))
