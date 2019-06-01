(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-io
  (:use #:common-lisp)
  (:export
   #:*io-readtable*
   #:save-info
   #:define-save-info
   #:read-model-object
   #:read-model
   #:write-model))

   
