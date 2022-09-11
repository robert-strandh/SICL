(cl:in-package #:asdf-user)

(defsystem #:sicl-arithmetic-floating-point
  :depends-on (#:sicl-arithmetic-run-time #:buoy)
  :serial t
  :components
  ((:file "floating-point-constants")
   (:file "floating-point")))
