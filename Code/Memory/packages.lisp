(cl:in-package #:common-lisp-user)

(defpackage #:sicl-memory
  (:use #:common-lisp)
  (:shadow #:car #:cdr #:class)
  (:export
   #:load-byte #:store-byte
   #:load-2-byte-word #:store-2-byte-word
   #:load-4-byte-word #:store-4-byte-word
   #:load-8-byte-word #:store-8-byte-word
   #:allocate-cons-cell
   #:allocate-heap-instance
   #:car #:cdr #:class #:contents-vector #:contents-element
   ))

