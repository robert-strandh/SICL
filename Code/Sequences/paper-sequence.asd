(cl:in-package #:asdf-user)

(defsystem :paper-sequence
  :serial t
  :components
  ((:file "Common/packages")
   (:file "Common/conditions")
   (:file "Common/condition-reporters-en")
   (:file "Common/docstrings-en")
   (:file "Common/utilities")
   (:file "Fast/paper-array-information")
   (:file "Fast/utilities")
   (:file "Common/find")
   (:file "Common/position")
   (:file "Common/count")))
