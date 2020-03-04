(cl:in-package #:common-lisp-user)

(defpackage #:sicl-utilities
  (:use #:common-lisp)
  (:export
   #:once-only
   #:with-gensyms
   #:with-collectors))
