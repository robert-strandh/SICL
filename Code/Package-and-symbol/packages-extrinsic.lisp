(cl:in-package #:common-lisp-user)

(defpackage sicl-package
  (:use #:common-lisp)
  (:shadow . #.asdf-user:*sicl-package-string-designators*)
  (:export . #.asdf-user:*sicl-package-string-designators*))
