(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hash-table
  (:use #:common-lisp)
  (:export #:*default-hash-table-class*
           #:eq-hash #:equal-hash #:equalp-hash
           #:%hash-table-test
           #:hashing-hash-table #:hash-table-hash-function #:hash-table-offset
           #:find-hash-function #:sxhash #:hash
           #:make-hash-table-iterator))
