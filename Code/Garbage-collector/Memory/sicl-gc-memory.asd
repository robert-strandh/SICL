(cl:in-package #:asdf-user)

(defsystem :sicl-gc-memory
  :serial t
  :components
  ((:file "packages")
   (:file "memory")))
