(cl:in-package #:common-lisp-user)

(defpackage #:sicl-sequence-test
  (:use #:common-lisp #:regression-test)
  (:shadow #:handler-case #:handler-bind)
  (:shadowing-import-from
   #:sicl-sequence
   . #.(loop for symbol being the external-symbols of '#:sicl-sequence
             collect symbol)))
