(cl:in-package #:asdf-user)

(defsystem #:sicl-client
  :serial t
  :components
  ((:file "packages")
   (:file "client")))
