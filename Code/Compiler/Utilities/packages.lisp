(cl:in-package #:common-lisp-user)

(defpackage #:sicl-compiler-utilities
  (:use #:common-lisp)
  (:export
   #:count-nodes
   #:depth-first-search-preorder
   #:depth-first-search-postorder
   #:predecessor-function
   ))
