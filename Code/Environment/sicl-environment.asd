(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-environment
  :depends-on (:sicl-code-utilities :cleavir-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "type-proclamations")
   (:file "environment-classes")
   (:file "create-environment")
   (:file "environment-constructors")
   (:file "environment-query")
   (:file "macroexpand")
   (:file "type-expand")
   (:file "fdefinition")
   (:file "special-operator-p")
   (:file "macro-function")
   (:file "compiler-macros")
   (:file "fully-expand-form")
   (:file "symbol-value")
   (:file "constantp")
   (:file "proclaim")))
