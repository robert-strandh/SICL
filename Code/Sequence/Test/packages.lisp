(cl:in-package #:common-lisp-user)

(defpackage #:sicl-sequence-test
  (:use #:common-lisp)
  (:shadow #:find #:find-if #:find-if-not
           #:position #:position-if #:position-if-not))
