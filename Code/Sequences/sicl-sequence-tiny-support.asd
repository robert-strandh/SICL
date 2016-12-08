(cl:in-package #:asdf-user)

(defsystem :sicl-sequence-tiny-support
  :serial t
  :components
  ((:file "Common/packages")
   (:file "Common/conditions")
   (:file "Common/condition-reporters-en")
   (:file "Common/docstrings-en")
   (:file "Common/utilities")
   (:file "Tiny/utilities")
   (:file "Common/find")
   (:file "Common/position")
   (:file "Common/count")))
