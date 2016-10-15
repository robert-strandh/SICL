(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-hir-transformations
  (:use #:common-lisp)
  (:export
   #:traverse
   #:load-time-value-is-constant-p
   #:load-time-value-constant
   #:convert-constant-to-immediate
   #:convert-constants-to-immediate
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
   #:eliminate-superfluous-temporaries
   #:maybe-eliminate))
