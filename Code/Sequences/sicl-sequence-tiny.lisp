(cl:in-package #:asdf-user)

(defsystem :sicl-sequence-tiny
  :depends-on (:lisp-unit)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "Tiny/sequences-tiny")
   (:file "condition-reporters-en")
   (:file "docstrings-en")))
