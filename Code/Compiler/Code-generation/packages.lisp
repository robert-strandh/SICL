(cl:in-package #:common-lisp-user)

(defpackage #:sicl-code-generation
  (:use #:common-lisp)
  (:local-nicknames (#:x86-64 #:sicl-x86-64-registers))
  (:export #:generate-code))
