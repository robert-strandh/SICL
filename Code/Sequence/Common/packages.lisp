(cl:in-package #:common-lisp-user)

(defpackage #:sicl-sequence
    (:use #:common-lisp)
  (:export #:name-mixin
           #:name
           #:in-sequence
           #:end-index
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
           #:end-less-than-start))
