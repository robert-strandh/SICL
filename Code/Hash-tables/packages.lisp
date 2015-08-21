(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hash-table
  (:use #:common-lisp)
  ;; Shadow these for now.  Ultimately, import them with
  ;; the rest of the CL package. 
  (:shadow #:hash-table
           #:make-hash-table #:hash-table-p
           #:hash-table-count #:hash-table-rehash-threshold #:hash-table-rehash-size
           #:hash-table-size #:hash-table-test
           #:puthash #:gethash #:remhash #:clrhash
           #:with-hash-table-iterator
           #:maphash))
