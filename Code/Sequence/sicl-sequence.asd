(cl:in-package #:asdf-user)

(defsystem :sicl-sequence
  :serial t
  :depends-on
  ("acclimation"
   "sicl-utilities"
   "sealable-metaobjects")
  :components
  ((:module "Generic"
    :components
    ((:file "packages")
     (:file "generic-functions")
     (:file "utilities")
     (:file "conditions")
     (:file "condition-reporters-en")
     (:file "for-each-relevant-cons")
     (:file "for-each-relevant-element")

     ;; First, load the a few elementary sequence functions and seal them.
     ;; This way, the subsequent definitions can already use optimized
     ;; versions of them.
     (:file "elt")
     (:file "length")
     (:file "adjust-sequence")
     (:file "make-sequence-like")
     (:file "seal-elementary-sequence-functions")

     ;; Now that all utilities and elementary sequence functions have been
     ;; defined, it is possible to load the bulk of the sequence functions.
     (:file "copy-seq")
     (:file "count")
     (:file "count-if")
     (:file "count-if-not")
     (:file "delete")
     (:file "delete-if")
     (:file "delete-if-not")
     (:file "fill")
     (:file "find")
     (:file "find-if")
     (:file "find-if-not")
     (:file "map-into")
     (:file "mismatch")
     (:file "nreverse")
     (:file "nsubstitute")
     (:file "nsubstitute-if")
     (:file "nsubstitute-if-not")
     (:file "reduce")
     (:file "remove-duplicates")
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
     (:file "substitute-if-not")
     (:file "seal-remaining-sequence-functions")))))

