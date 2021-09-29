(cl:in-package #:asdf-user)

(defsystem sicl-random
  :serial t
  :components
  ((:file "packages")
   (:file "random-state-defclass")
   (:file "random")))
