(cl:in-package #:asdf-user)

(defsystem #:sicl-array-proclamations
  :depends-on (#:sicl-array-support)
  :serial t
  :components
  ((:file "proclamations")))
