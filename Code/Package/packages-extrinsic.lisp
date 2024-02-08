(cl:in-package #:common-lisp-user)

(defpackage sicl-package
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:shadow . #.asdf-user:*sicl-package-string-designators*)
  (:export . #.asdf-user:*sicl-package-string-designators*))
