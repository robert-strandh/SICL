(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hir-evaluator-test
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:test))
