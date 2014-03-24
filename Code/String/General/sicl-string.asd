(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-string
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
