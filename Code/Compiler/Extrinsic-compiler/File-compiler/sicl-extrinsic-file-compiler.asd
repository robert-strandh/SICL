(cl:in-package #:asdf-user)

(defsystem :sicl-extrinsic-file-compiler
  :depends-on (:sicl-reader-simple)
  :serial t
  :components
  ((:file "packages")))
