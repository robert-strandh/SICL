(cl:in-package #:common-lisp-user)

(defpackage #:sicl-evaluation-and-compilation
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:declaim-expander))
