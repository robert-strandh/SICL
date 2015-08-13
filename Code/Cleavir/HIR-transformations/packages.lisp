(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-hir-transformations
  (:use #:common-lisp)
  (:export
   #:traverse
   #:convert-constant-to-immediate
   #:eliminate-load-time-value-inputs
   #:eliminate-typeq
   #:type-inference
   #:process-captured-variables
   #:segregate-only
   #:compute-instruction-owners
   #:compute-location-owners
   #:segregate-lexicals
   #:hir-transformations
   #:introduce-intermediate
   #:eliminate-superfluous-temporaries))
