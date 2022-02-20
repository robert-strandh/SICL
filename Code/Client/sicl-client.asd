(cl:in-package #:asdf-user)

(defsystem #:sicl-client
  :depends-on (#:trucler-reference
               #:clostrum
               #:clostrum-basic)
  :serial t
  :components
  ((:file "packages")
   (:file "client")))
