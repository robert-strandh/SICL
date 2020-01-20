(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hash-table
  (:use #:common-lisp)
  (:export #:*default-hash-table-class* #:make-hash-table-iterator))
