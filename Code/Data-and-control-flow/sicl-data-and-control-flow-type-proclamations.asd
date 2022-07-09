(cl:in-package #:asdf-user)

(defsystem :sicl-data-and-control-flow-type-proclamations
  :depends-on (:sicl-data-and-control-flow-support)
  :serial t
  :components
  ((:file "type-proclamations")))
