(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot
  :depends-on (:sicl-hir-to-cl)
  :serial t
  :components
  ((:file "packages")))
