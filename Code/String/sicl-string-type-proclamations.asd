(cl:in-package #:asdf-user)

(defsystem #:sicl-string-type-proclamations
  :depends-on (#:sicl-string-support)
  :serial t
  :components
  ((:file "type-proclamations")))
