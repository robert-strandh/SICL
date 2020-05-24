(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hash-table
  (:use #:common-lisp)
  (:export #:*default-hash-table-class*
           #:eq-hash #:equal-hash #:equalp-hash
           #:%hash-table-test
           #:find-hash-function #:sxhash
           #:make-hash-table-iterator))
