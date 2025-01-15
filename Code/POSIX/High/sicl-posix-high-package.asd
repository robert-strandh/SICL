(cl:in-package #:asdf-user)

(defsystem "sicl-posix-high-package"
  :depends-on ("sicl-posix-low-package")
  :serial t
  :components
  ((:file "packages")))
