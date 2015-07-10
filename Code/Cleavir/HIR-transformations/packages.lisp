(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-hir-transformations
  (:use #:common-lisp)
  (:export
   #:traverse
   #:eliminate-typeq
   #:type-inference
   #:process-captured-variables
   #:segregate-lexicals
   #:hir-transformations
   #:introduce-intermediate
   #:eliminate-superfluous-temporaries))
