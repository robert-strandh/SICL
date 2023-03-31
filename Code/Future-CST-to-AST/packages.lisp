(cl:in-package #:common-lisp-user)

(defpackage #:sicl-expression-to-ast
  (:use #:common-lisp)
  (:shadow #:eval)
  (:local-nicknames
   (#:ico #:iconoclast)
   (#:bld #:iconoclast-builder)
   (#:abp #:architecture.builder-protocol)
   (#:c #:sicl-extended-clearcut))
  (:export #:expression-to-ast))
