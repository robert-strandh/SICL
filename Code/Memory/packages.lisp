(cl:in-package #:common-lisp-user)

(defpackage #:sicl-memory
  (:use #:common-lisp)
  (:export #:load-unsigned
           #:store-unsigned
           #:load-signed
           #:store-signed))
