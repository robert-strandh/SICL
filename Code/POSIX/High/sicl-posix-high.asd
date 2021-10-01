(cl:in-package #:asdf-user)

(defsystem #:sicl-posix-high
  :depends-on (#:sicl-posix-low)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "exit")
   (:file "read")
   (:file "write")))
