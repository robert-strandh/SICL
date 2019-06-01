(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase-0
  (:use #:common-lisp)
  (:shadow #:load-file #:compile-file)
  (:export))
