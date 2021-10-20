(cl:in-package #:common-lisp-user)

(defpackage sicl-package
  (:use #:common-lisp)
  (:export . #.asdf-user:*sicl-package-string-designators*))
