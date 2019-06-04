(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-base
  :depends-on (#:sicl-alternative-extrinsic-environment
               #:sicl-ast-to-hir
               #:sicl-hir-to-cl)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "utilities")))
