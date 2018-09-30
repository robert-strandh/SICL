(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-load-time-value-hoisting
  (:use #:common-lisp)
  (:export
   #:hoist-load-time-values

   #:simplify-datum
   #:hir-from-form
   #:make-load-form-using-client
   #:equalp-keys

   #:scan-hir
   #:scan-datum
   #:scan-literal-object

   #:hoist-toplevel-hir
   #:hoist-hir
   #:hoist-datum))
