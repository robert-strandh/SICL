(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-hir-interpreter
  (:use #:common-lisp)
  (:export
   #:translate
   #:compile-hir
   #:interpret-hir
   ))
