(cl:in-package #:common-lisp-user)

(defpackage #:sicl-conditions
  (:use #:common-lisp)
  (:export
   #:define-condition-expander
   #:condition-class))
