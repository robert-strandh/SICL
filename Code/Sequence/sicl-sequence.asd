(cl:in-package #:asdf-user)

(defsystem :sicl-sequence
  :serial t
  :depends-on
  ("acclimation"
   "sicl-utilities"
   "fast-generic-functions")
  :components
  ((:module "Generic"
    :components
    ((:file "packages")
     (:file "sequence-function")
     (:file "generic-functions")
     (:file "utilities")
     (:file "conditions")
     (:file "condition-reporters-en")

     ;; Firstly, load the a few elementary sequence functions and seal
     ;; them.  This way, the subsequent definitions can already use
     ;; optimized versions of them.
     (:file "length")
     (:file "elt")
     (:file "adjust-sequence")
     (:file "make-sequence")
     (:file "make-sequence-like")

     ;; Secondly, load several high-level utilities for working with sequences.
     (:file "for-each-relevant-cons")
     (:file "for-each-relevant-element")
     (:file "make-cons-iterator")
     (:file "make-sequence-reader")
     (:file "make-sequence-writer")

     ;; Now that all utilities and elementary sequence functions have been
     ;; defined, it is possible to load the remaining functions.
     (:file "copy-seq")
     (:file "concatenate")
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
     (:file "map-aux")
     (:file "map-into")
     (:file "map")
     (:file "merge")
     (:file "mismatch")
     (:file "nreverse")
     (:file "nsubstitute-aux")
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
     (:file "substitute-if-not")))))

