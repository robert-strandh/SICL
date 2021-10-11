(cl:in-package #:common-lisp-user)

(defpackage #:sicl-allocator
  (:use #:common-lisp)
  (:export #:initialize-heap
           #:allocate-dyad
           #:allocate-chunk
           #:free))
