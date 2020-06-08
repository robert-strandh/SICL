(cl:in-package #:common-lisp-user)

(defpackage #:sicl-utilities
  (:use #:common-lisp)
  (:export
   #:flp2
   #:clp2
   #:once-only
   #:with-gensyms
   #:with-collectors))
