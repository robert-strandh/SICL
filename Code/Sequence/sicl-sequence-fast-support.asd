(cl:in-package #:asdf-user)

(defsystem :sicl-sequence-fast-support
  :serial t
  :components
  ((:file "Common/packages")
   (:file "Common/conditions")
   (:file "Common/condition-reporters-en")
   (:file "Common/docstrings-en")
   (:file "Common/utilities")
   (:file "Fast/special-array-information")
   (:file "Fast/utilities")
   (:file "Common/find")
   (:file "Common/position")
   (:file "Common/count")))
