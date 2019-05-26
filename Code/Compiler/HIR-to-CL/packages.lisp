(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hir-to-cl
  (:use #:common-lisp)
  (:export #:hir-to-cl
           #:cst-eval))
