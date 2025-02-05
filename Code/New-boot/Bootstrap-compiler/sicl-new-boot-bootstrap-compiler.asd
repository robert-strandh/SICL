(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-bootstrap-compiler"
  :depends-on ("sicl-new-boot")
  :serial t
  :components
  ((:file "packages")))
