(cl:in-package #:common-lisp-user)

(defpackage #:sicl-register-allocation
  (:use #:common-lisp)
  (:local-nicknames (#:arr #:sicl-register-arrangement))
  (:export #:do-register-allocation))
