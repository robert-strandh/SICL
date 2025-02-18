(cl:in-package #:asdf-user)

(defsystem "sicl-ast-to-hir-test"
  :depends-on ("sicl-ast-to-hir"
               "common-boot")
  :serial t
  :components
  ((:file "packages")
   (:file "configuration")
   (:file "utilities")))
