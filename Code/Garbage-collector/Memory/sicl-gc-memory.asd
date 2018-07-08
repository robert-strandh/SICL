(cl:in-package #:asdf-user)

(defsystem :sicl-gc-memory
  :components
  ((:file "packages")
   (:file "memory")))
