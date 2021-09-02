(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hir-interpreter-test
  (:use #:common-lisp)
  (:shadow #:eval)
  (:export #:test))
