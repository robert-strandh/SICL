(cl:in-package #:common-lisp-user)

(defpackage #:sicl-memory
  (:use #:common-lisp)
  (:export #:memory-unsigned
           #:memory-signed
           #:object-to-fixnum
           #:fixnum-to-object))
