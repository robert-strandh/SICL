(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-inspector
  :depends-on (#:sicl-boot
               #:new-inspector)
  :serial t
  :components
  ((:file "packages")
   (:file "function")))
