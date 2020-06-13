(cl:in-package #:common-lisp-user)

(defpackage #:sicl-list-hash-table
  (:use #:common-lisp)
  (:import-from
   #:sicl-hash-table
   #:make-hash-table-iterator
   #:%hash-table-test))
