(cl:in-package #:asdf-user)

(defsystem sicl-random
  :serial t
  :components
  ((:file "packages")
   (:file "random-state-defclass")
   (:file "random-state-defgeneric")
   (:file "mersenne-twister")
   (:file "pcg32")
   (:file "random")))
