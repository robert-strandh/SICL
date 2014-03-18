(cl:in-package #:common-lisp-user)

(defpackage #:sicl-sequence
  (:use #:common-lisp)
  (:export
   #:find #:find-if #:find-if-not
   #:count #:count-if #:count-if-not
   #:remove #:remove-if #:remove-if-not
   #:delete #:delete-if #:delete-if-not
   #:sort #:stable-sort))
