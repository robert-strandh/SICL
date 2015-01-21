(in-package #:asdf-user)

(defsystem :sicl-iteration
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "iteration")
   (:file "condition-reporters-en")
   (:file "docstrings-en")))
