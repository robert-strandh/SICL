(cl:in-package #:asdf-user)

(defsystem #:cleavir-code-utilities-type-proclamations
  :depends-on (#:cleavir-code-utilities-base)
  :serial t
  :components
  ((:file "type-proclamations")))
