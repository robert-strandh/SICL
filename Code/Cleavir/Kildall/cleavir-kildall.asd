(cl:in-package #:asdf-user)

;;;; Implementation of Kildall's "Algorithm A":
;;;;
;;;; G.A. Kildall, "A Unified Approach to Global Program
;;;; Optimization." Proceedings of the First ACM Symposium on
;;;; Principles of Programming Languages,194-206, 1973.
;;;;
;;;; It's a very general algorithm for optimization information on
;;;; program structures like HIR.
;;;; See liveness.lisp for an example.

(defsystem :cleavir-kildall
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "kildall")
   (:file "pool")
   (:file "dictionary")
   (:file "work-list")
   (:file "map-pool")
   (:file "iterate")
   (:file "initial-work")
   (:file "alist-pool")
   (:file "bitset")
   (:file "interfunction")))
