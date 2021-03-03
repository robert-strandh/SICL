(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hir-to-mir
  (:use #:common-lisp)
  (:export #:hir-to-mir
           #:gather-enter-instructions
           #:enter-instruction))
