(cl:in-package #:common-lisp-user)

(defpackage #:sicl-call-site-manager
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:export
   #:create-trampoline-snippet))
