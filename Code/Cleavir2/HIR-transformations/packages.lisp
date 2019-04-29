(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-hir-transformations
  (:use #:common-lisp)
  (:export
   #:segregate-only
   #:segregate-lexicals
   #:build-function-dag))
