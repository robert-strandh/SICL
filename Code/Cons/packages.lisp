(cl:in-package #:common-lisp-user)

(defpackage #:sicl-cons
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:pushnew-expander
           #:push-expander
           #:pop-expander
           #:remf-expander))
