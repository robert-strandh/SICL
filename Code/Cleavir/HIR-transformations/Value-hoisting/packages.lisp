(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-value-hoisting
  (:use #:common-lisp)
  (:export
   #:hoist-values

   #:simplify-datum
   #:hir-from-form
   #:make-load-form-using-client
   #:equal-representation
   #:equalp-representation
   #:similarity-keys

   #:scan-hir
   #:scan-datum
   #:scan-literal-object

   #:hoist-toplevel-hir
   #:hoist-hir
   #:hoist-datum))
