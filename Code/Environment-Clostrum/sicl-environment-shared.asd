(cl:in-package #:asdf-user)

(defsystem "sicl-environment-shared"
  :serial t
  :components
  ((:file "variables")
   (:file "fdefinition")
   (:file "find-class")
   (:file "macro-function")
   (:file "compiler-macro-function")
   (:file "boundp")
   (:file "symbol-value")))
