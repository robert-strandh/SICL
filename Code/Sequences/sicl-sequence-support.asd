(cl:in-package #:asdf-user)

(defsystem :sicl-sequence-support
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "conditions")
   (:file "find")
   (:file "position")
   (:file "count")
   (:file "fill")))
