(cl:in-package #:asdf-user)

(defsystem :sicl-sequence
  :depends-on (:lisp-unit)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "sequences")
   (:file "condition-reporters-en")
   (:file "docstrings-en")))
