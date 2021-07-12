(cl:in-package #:asdf-user)

(defsystem :sicl-string-test
  :serial t
  :components
  ((:file "packages")
   (:file "string-type")
   (:file "conditions")
   (:file "condition-reporters-en")
   (:file "utilities")
   (:file "copy")
   (:file "case-conversion")
   (:file "string")
   (:file "trim")
   (:file "comparison")
   (:file "test")))
