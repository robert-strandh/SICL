(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-hir-transformations
  (:use #:common-lisp)
  (:export
   #:eliminate-typeq
   #:type-inference
   #:process-fdefinitions
   #:process-fdefinition
   #:process-captured-variables
   #:compute-ownerships
   #:segregate-lexicals
   #:hir-transformations
   #:introduce-immediate))
