(cl:in-package #:asdf-user)

(defsystem #:sicl-client
  :depends-on (#:trucler-reference
               #:clostrum
               #:clostrum/virtual)
  :serial t
  :components
  ((:file "packages")
   (:file "client")))
