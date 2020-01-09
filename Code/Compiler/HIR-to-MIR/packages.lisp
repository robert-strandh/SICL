(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hir-to-mir
  (:use #:common-lisp)
  (:export #:hir-to-mir
           #:entry-point-input
           #:enter-instruction
           +code-object-index+
           +enclose-function-index+
           +cons-function-index+
           +nil-index+
           +first-constant-index+))
