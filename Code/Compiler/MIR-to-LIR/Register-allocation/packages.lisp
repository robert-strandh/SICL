(cl:in-package #:common-lisp-user)

(defpackage #:sicl-register-allocation
  (:use #:common-lisp)
  (:local-nicknames (#:arr #:sicl-register-arrangement)
                    (#:x86-64 #:sicl-x86-64-registers))
  (:export #:do-register-allocation #:adapt-instruction))
