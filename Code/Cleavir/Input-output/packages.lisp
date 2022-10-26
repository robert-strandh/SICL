(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-io
  (:use #:common-lisp)
  (:export
   #:*io-readtable*
   #:cloneable-mixin
   #:read-model-object
   #:read-model
   #:write-model))
