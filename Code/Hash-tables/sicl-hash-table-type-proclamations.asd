(cl:in-package #:asdf-user)

(defsystem #:sicl-hash-table-type-proclamations
  :depends-on (#:sicl-hash-table-base)
  :serial t
  :components
  ((:file "type-proclamations")))
