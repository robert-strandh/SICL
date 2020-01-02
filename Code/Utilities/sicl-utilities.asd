(cl:in-package #:asdf-user)

(defsystem :sicl-utilities
  :serial t
  :components
  ((:file "packages")
   (:file "once-only")
   (:file "with-collectors")))
