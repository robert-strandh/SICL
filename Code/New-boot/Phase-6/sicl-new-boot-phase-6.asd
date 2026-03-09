(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-phase-6"
  :depends-on ("buoy-simulate")
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "configuration")
   (:file "macro-programming")
   (:file "floats")
   (:file "load-quaviver")
   (:file "boot")))
