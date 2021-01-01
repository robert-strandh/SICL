(cl:in-package #:common-lisp-user)

(defpackage #:sicl-linear-probing-hash-table
  (:shadowing-import-from
   #:sicl-hash-table
   #:hashing-hash-table
   #:make-hash-table #:hash-table-p
   #:hash-table-count #:hash-table-rehash-threshold #:hash-table-rehash-size
   #:hash-table-size #:hash-table-test #:%hash-table-test
   #:gethash #:remhash #:clrhash
   #:find-hash-function
   #:maphash)
  (:use #:common-lisp)
  (:export #:linear-probing-hash-table))
