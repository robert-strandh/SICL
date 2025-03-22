(cl:in-package #:asdf-user)

(defsystem "sicl-arithmetic-bootstrap-methods"
  :depends-on ()
  :serial t
  :components
  ((:file "simulated-single-float-methods")
   (:file "simulated-double-float-methods")))
