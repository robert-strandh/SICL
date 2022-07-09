(cl:in-package #:asdf-user)

(defsystem :sicl-data-and-control-flow-compile-time
  :depends-on (#:sicl-data-and-control-flow-support
               #:sicl-data-and-control-flow-macros
               #:sicl-data-and-control-flow-type-proclamations)
  :serial t
  :components ())
