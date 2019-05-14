(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hir-to-cl-test
  (:use #:common-lisp)
  (:shadow #:eval)
  (:export #:test))
