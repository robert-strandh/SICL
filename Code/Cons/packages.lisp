(cl:in-package #:common-lisp-user)

(defpackage #:sicl-cons
  (:use #:common-lisp)
  (:export #:pushnew-expander
           #:push-expander
           #:pop-expander))
