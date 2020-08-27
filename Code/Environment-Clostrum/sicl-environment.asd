(cl:in-package #:asdf-user)

(defsystem #:sicl-environment
  :depends-on (#:trucler
               #:trucler-reference
               #:clostrum
               #:clostrum/virtual)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "trucler-methods")))
