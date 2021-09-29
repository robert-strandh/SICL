(cl:in-package #:asdf-user)

(defsystem sicl-random-intrinsic
  :serial t
  :components
  ((:file "packages-intrinsic")
   (:file "random-state-defclass")
   (:file "random")))
