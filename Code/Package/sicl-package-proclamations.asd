(cl:in-package #:asdf-user)

(defsystem :sicl-package-proclamations
  :serial t
  :components
  ((:file "packages")
   (:file "proclamations")))
