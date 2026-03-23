(cl:in-package #:asdf-user)

(defsystem #:sicl-arithmetic-floating-point
  :depends-on ()
  :serial t
  :components
  ((:file "floating-point-constants")
   (:file "floating-point")
   (:file "log")
   (:file "exp")
   (:file "sin")
   (:file "cos")
   (:file "asin")
   (:file "acos")
   (:file "atan")
   (:file "sinh")
   (:file "cosh")
   (:file "tanh")))
