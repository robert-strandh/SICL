(cl:in-package #:asdf-user)

(defsystem #:sicl-string
  :depends-on (#:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-en")
   (:file "utilities")
   (:file "copy")
   (:file "case-conversion")
   (:file "string")
   (:file "trim")
   (:file "comparison")))
