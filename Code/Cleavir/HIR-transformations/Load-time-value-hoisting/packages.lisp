(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-load-time-value-hoisting
  (:use #:common-lisp)
  (:export
   #:hoist-load-time-values
   #:make-constructor
   #:compile-form
   #:scan-hir
   #:scan-datum
   #:scan-literal-object
   #:equalp-keys
   #:hoist-toplevel-hir
   #:hoist-hir
   #:hoist-datum))
