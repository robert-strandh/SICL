(cl:in-package #:asdf-user)

(defsystem "sicl-environment-shared"
  :serial t
  :components
  ((:file "variables")
   (:file "fdefinition")
   (:file "find-class")))
