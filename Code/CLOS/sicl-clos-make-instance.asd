(cl:in-package #:asdf-user)

(defsystem "sicl-clos-make-instance"
  :depends-on ("sicl-clos-package")
  :components
  ((:file "make-instance")))
