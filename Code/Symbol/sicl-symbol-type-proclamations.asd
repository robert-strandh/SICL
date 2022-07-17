(cl:in-package #:asdf-user)

(defsystem #:sicl-symbol-type-proclamations
  :depends-on (#:sicl-symbol-support)
  :serial t
  :components ((:file "type-proclamations")))
