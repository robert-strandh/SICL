(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-bootstrap-compiler"
  :depends-on ("sicl-new-boot"
               "common-boot-fast-ast-evaluator")
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "translate-source-file")))
