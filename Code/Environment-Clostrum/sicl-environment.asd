(cl:in-package #:asdf-user)

(defsystem #:sicl-environment
  :depends-on (#:clostrum
               #:clostrum/virtual)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")))
