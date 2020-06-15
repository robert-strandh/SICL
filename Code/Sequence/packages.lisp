(cl:in-package #:common-lisp-user)

(defpackage #:sicl-sequence
  (:use #:closer-common-lisp)

  (:import-from
   #:fast-generic-functions
   #:fast-generic-function
   #:seal-domain
   #:sealed-domains
   #:method-properties
   #:inlineable
   #:no-primary-method)

  (:shadow
   ;; We define LIST-LENGTH as the type of integers that denote the length
   ;; of a list.
   #:list-length
   . #1=
   (#:copy-seq
    #:elt
    #:fill
    #:make-sequence
    #:subseq
    #:map-into
    #:map
    #:reduce
    #:count
    #:count-if
    #:count-if-not
    #:length
    #:reverse
    #:nreverse
    #:sort
    #:stable-sort
    #:find
    #:find-if
    #:find-if-not
    #:position
    #:position-if
    #:position-if-not
    #:search
    #:mismatch
    #:replace
    #:substitute
    #:substitute-if
    #:substitute-if-not
    #:nsubstitute
    #:nsubstitute-if
    #:nsubstitute-if-not
    #:concatenate
    #:merge
    #:remove
    #:remove-if
    #:remove-if-not
    #:delete
    #:delete-if
    #:delete-if-not
    #:remove-duplicates
    #:delete-duplicates))
  (:export
   ;; Conditions.
   #:must-be-nonnegative-integer
   #:must-be-cons
   #:must-be-sequence
   #:must-be-function-designator
   #:must-be-list
   #:must-be-proper-list
   #:must-be-recognizable-subtype-of-sequence
   #:must-be-recognizable-subtype-of-vector
   #:invalid-sequence-index
   #:invalid-bounding-index
   #:invalid-start-index
   #:invalid-end-index
   #:end-less-than-start
   #:both-test-and-test-not-given
   #:warn-both-test-and-test-not-given

   ;; Non-standard symbols.
   #:make-sequence-like
   #:adjust-sequence

   ;; Standard symbols.
   . #1#))
