(cl:in-package #:common-lisp-user)

(defpackage #:sicl-additional-conditions
  (:use #:common-lisp #:sicl-additional-types)
  (:export
   #:sicl-condition
   #:sicl-warning
   #:sicl-style-warning
   #:sicl-error
   #:sicl-type-error
   #:sicl-cell-error
   #:both-test-and-test-not-given
   #:warn-both-test-and-test-not-given
   #:list-as-sequence-must-be-proper
   #:invalid-bounding-indexes
   #:invalid-start-index
   #:invalid-end-index
   #:sicl-program-error
   #:sicl-program-warning
   #:sicl-program-style-warning
   #:sicl-unbound-function)
  (:shadow #:sequence))
