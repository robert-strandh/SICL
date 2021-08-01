(cl:in-package #:common-lisp-user)

(defpackage sicl-package
  (:use #:common-lisp)
  (:shadow #:find-package)
  (:export . #.asdf-user:*sicl-package-string-designators*))
