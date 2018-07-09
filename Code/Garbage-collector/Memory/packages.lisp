(cl:in-package #:common-lisp-user)

(defpackage #:sicl-gc-memory 
  (:use #:common-lisp)
  (:export #:memory-64
           #:memory-32
           #:memory-16
           #:memory-8
           #:init-memory
           #:end-memory))
