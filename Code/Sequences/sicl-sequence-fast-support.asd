(cl:in-package #:asdf-user)

(defsystem :sicl-sequence-fast-support
  :serial t
  :components
  ((:file "Common/packages")
   (:file "Common/conditions")
   (:file "Common/condition-reporters-en")
   (:file "Common/docstrings-en")
   (:file "Common/utilities")
   (:file "Fast/utilities")
   (:file "Fast/find")
   (:file "Fast/position")
   (:file "Fast/count")))
