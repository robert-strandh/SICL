(cl:in-package #:common-lisp-user)

(defpackage sicl-package
  (:use #:common-lisp)
  (:shadow #:find-package
           #:delete-package
           #:make-symbol)
  (:export . #.asdf-user:*sicl-package-string-designators*))
