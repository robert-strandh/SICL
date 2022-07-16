(cl:in-package #:asdf-user)

(defsystem #:sicl-conditions-type-proclamations
  :depends-on (#:sicl-conditions-support)
  :serial t
  :components
  ((:file "type-proclamations")))
