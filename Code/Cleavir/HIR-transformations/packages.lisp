(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-hir-transformations
  (:use #:common-lisp)
  (:export
   #:traverse
   #:eliminate-typeq
   #:type-inference
   #:process-captured-variables
   #:compute-ownerships
   #:segregate-lexicals
   #:hir-transformations
   #:introduce-intermediate))
