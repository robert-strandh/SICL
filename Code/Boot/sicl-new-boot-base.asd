(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-base
  :depends-on (#:sicl-extrinsic-environment
               #:sicl-ast-to-hir
               #:sicl-hir-to-cl)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "boot-class")
   (:file "utilities")))
