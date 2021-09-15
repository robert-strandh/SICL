(cl:in-package #:asdf-user)

(defsystem :sicl-sequence-for-sicl-boot
  :serial t
  :depends-on
  ("acclimation"
   "sicl-utilities"
   "fast-generic-functions")

  :components
  ((:file "packages-intrinsic")

   ;; Load all types, conditions, and utilities.
   (:file "types")
   (:file "conditions")
   (:file "condition-reporters-en")
   (:file "sequence-function")
   (:file "generic-functions")

   ;; Load the a few elementary sequence functions and seal
   ;; them.  This way, the subsequent definitions can already use
   ;; optimized versions of them.
   (:file "length")
   (:file "elt")

   ;; Load utilities.
   (:file "utilities")
   (:file "with-cons-iterator")
   (:file "with-key-function")
   (:file "with-list-start-and-end")
   (:file "with-predicate")
   (:file "with-reified-result-type")
   (:file "with-test-function")
   (:file "with-vector-start-and-end")
   (:file "for-each-relevant-cons")
   (:file "for-each-relevant-element")
   (:file "sequence-scanner")

   ;; Now that all utilities and elementary sequence functions have been
   ;; defined, it is possible to load the remaining functions.  The order
   ;; is mostly alphabetical, except that some very important functions
   ;; like MAKE-SEQUENCE and NREVERSE are loaded early.
   (:file "adjust-sequence")
   (:file "make-sequence")
   (:file "make-sequence-like")
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

