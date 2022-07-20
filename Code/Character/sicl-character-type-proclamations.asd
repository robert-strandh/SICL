(cl:in-package #:asdf-user)

(defsystem #:sicl-character-type-proclamations
  :depends-on (#:sicl-character-support)
  :serial t
  :components
  ((:file "type-proclamations")))
