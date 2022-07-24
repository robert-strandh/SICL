(cl:in-package #:asdf-user)

(defsystem #:sicl-cons-type-proclamations
  :depends-on (#:sicl-cons-package)
  :serial t
  :components
  ((:file "type-proclamations")))
