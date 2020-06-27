(cl:in-package #:asdf-user)

(defsystem :sicl-sequence
  :serial t
  :depends-on
  ("acclimation"
   "sicl-utilities"
   "fast-generic-functions")
  :components
  ((:file "packages")

   ;; Load all types, conditions, and utilities.
   (:file "types")
   (:file "conditions")
   (:file "condition-reporters-en")
   (:file "sequence-function")
   (:file "generic-functions")
   (:file "utilities")
   (:file "with-key-function")
   (:file "with-predicate")
   (:file "with-test-function")
   (:file "with-reified-result-type")

   ;; Load the a few elementary sequence functions and seal
   ;; them.  This way, the subsequent definitions can already use
   ;; optimized versions of them.
   (:file "length")
   (:file "elt")
   (:file "adjust-sequence")
   (:file "make-sequence")
   (:file "make-sequence-like")

   ;; Load several high-level utilities for working with sequences.
   (:file "for-each-relevant-cons")
   (:file "for-each-relevant-element")
   (:file "make-cons-iterator")
   (:file "sequence-scanner")

   ;; Now that all utilities and elementary sequence functions have been
   ;; defined, it is possible to load the remaining functions.
   (:file "copy-seq")
   (:file "nreverse")
   (:file "count")
   (:file "count-if")
   (:file "count-if-not")
   (:file "delete-aux")
   (:file "delete-duplicates")
   (:file "delete")
   (:file "delete-if")
   (:file "delete-if-not")
   (:file "fill")
   (:file "find")
   (:file "find-if")
   (:file "find-if-not")
   (:file "map-for-effect")
   (:file "map-into")
   (:file "map")
   (:file "concatenate")
   (:file "merge")
   (:file "mismatch")
   (:file "nsubstitute")
   (:file "nsubstitute-if")
   (:file "nsubstitute-if-not")
   (:file "position-if")
   (:file "position-if-not")
   (:file "position")
   (:file "reduce")
   (:file "remove-duplicates")
   (:file "remove-aux")
   (:file "remove-if")
   (:file "remove-if-not")
   (:file "remove")
   (:file "replace")
   (:file "reverse")
   (:file "search")
   (:file "sort")
   (:file "stable-sort")
   (:file "subseq")
   (:file "substitute")
   (:file "substitute-if")
   (:file "substitute-if-not")))

