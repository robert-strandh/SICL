(cl:in-package #:asdf-user)

(defsystem #:sicl-register-arrangement
  :serial t
  :components
  ((:file "packages")
   (:file "arrangement")))
