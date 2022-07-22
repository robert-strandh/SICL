(cl:in-package #:asdf-user)

(defsystem #:sicl-clos-type-proclamations
  :depends-on (#:sicl-clos-package)
  :serial t
  :components
  ((:file "type-proclamations")))
