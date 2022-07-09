(cl:in-package #:asdf-user)

(defsystem #:sicl-arithmetic-type-proclamations
  :depends-on (#:sicl-arithmetic-base)
  :serial t
  :components
  ((:file "type-proclamations")))
