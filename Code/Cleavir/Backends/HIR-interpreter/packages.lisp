(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-hir-interpreter
  (:use #:common-lisp)
  (:export
   #:compile-hir
   #:interpret-hir
   ))
