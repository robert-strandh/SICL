(cl:in-package #:common-lisp-user)

(defpackage #:sicl-new-boot
  (:use #:common-lisp)
  (:local-nicknames
   (#:cb #:common-boot)
   (#:cmd #:common-macro-definitions))
  (:export))
