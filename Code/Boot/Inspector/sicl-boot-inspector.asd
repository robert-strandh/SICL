(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-inspector
  :depends-on (#:sicl-boot #:clouseau)
  :components
  ((:file "packages")
   (:file "methods")))
