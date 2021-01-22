(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hash-table
  (:use #:common-lisp)
  ;; Shadow these for now.  Ultimately, import them with
  ;; the rest of the CL package. 
  (:shadow #:sxhash #:hash
           #:hash-table
           #:make-hash-table #:hash-table-p
           #:hash-table-count #:hash-table-rehash-threshold #:hash-table-rehash-size
           #:hash-table-size #:hash-table-test
           #:gethash #:remhash #:clrhash
           #:with-hash-table-iterator
           #:maphash)
  (:export #:*default-hash-table-class*
           #:sxhash #:hash #:eq-hash #:equal-hash #:equalp-hash #:find-hash-function
           #:hash-table #:hashing-hash-table
           #:hash-table-hash-function #:hash-table-offset
           #:make-hash-table #:hash-table-p
           #:hash-table-count #:hash-table-rehash-threshold #:hash-table-rehash-size
           #:hash-table-size #:hash-table-test #:%hash-table-test
           #:gethash #:remhash #:clrhash
           #:with-hash-table-iterator
           #:maphash
           #:make-hash-table-iterator))
