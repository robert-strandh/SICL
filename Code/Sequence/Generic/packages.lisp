(cl:in-package #:common-lisp-user)

(defpackage #:sicl-sequence
  (:use #:closer-common-lisp)

  (:import-from
   #:fast-generic-functions
   #:fast-generic-function
   #:seal-domain
   #:method-properties
   #:inlineable
   #:no-primary-method)

  (:shadow
   . #1=
   (#:copy-seq
    #:elt
    #:fill
    #:subseq
    #:map-into
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
   #:must-be-sequence
   #:must-be-cons
   #:must-be-list
   #:must-be-proper-list
   #:both-test-and-test-not-given
   #:warn-both-test-and-test-not-given
   #:invalid-sequence-index-type
   #:invalid-start-index-type
   #:invalid-end-index-type
   #:invalid-sequence-index
   #:invalid-bounding-index
   #:invalid-start-index
   #:invalid-end-index
   #:end-less-than-start

   ;; Non-standard symbols.
   #:make-sequence-like
   #:adjust-sequence

   ;; Standard symbols.
   . #1#))
