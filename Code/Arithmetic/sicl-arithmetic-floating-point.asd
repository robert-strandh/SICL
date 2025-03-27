(cl:in-package #:asdf-user)

(defsystem #:sicl-arithmetic-floating-point
  :depends-on ()
  :serial t
  :components
  ((:file "bits-to-float")
   (:file "floating-point-constants")))
