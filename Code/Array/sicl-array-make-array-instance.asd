(cl:in-package #:asdf-user)

(defsystem "sicl-array-make-array-instance"
  :depends-on ("sicl-array-support")
  :serial t
  :components
  ((:file "make-array-instance")))
