(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-inspector
  :depends-on (#:sicl-new-boot #:clouseau)
  :components
  ((:file "packages")))
