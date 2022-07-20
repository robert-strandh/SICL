(cl:in-package #:asdf-user)

(defsystem #:sicl-type-type-proclamations
  :depends-on (#:sicl-type-support)
  :components
  ((:file "type-proclamations")))
