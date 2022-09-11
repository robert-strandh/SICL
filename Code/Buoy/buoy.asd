(cl:in-package #:asdf-user)

(defsystem :buoy
  :serial t
  :depends-on ("float-features")
  :components ((:file "package")
               (:file "buoy")))
