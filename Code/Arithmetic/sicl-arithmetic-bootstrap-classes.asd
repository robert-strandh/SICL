(cl:in-package #:asdf-user)

(defsystem #:sicl-arithmetic-bootstrap-classes
  :depends-on (#:sicl-arithmetic-base)
  :serial t
  :components
  ((:file "simulated-single-float-defclass")))
