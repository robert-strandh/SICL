(cl:in-package #:common-lisp-user)

(defpackage #:sicl-standard-environment-functions
  (:shadow #:variable)
  (:use #:common-lisp))

(defpackage #:sicl-standard-environment-macros
  (:use #:common-lisp)
  (:export #:deftype-expander
           #:define-compiler-macro-expander))
