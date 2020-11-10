(cl:in-package #:asdf-user)

(defsystem #:sicl-posix-high
  :depends-on (#:sicl-posix-low)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "write")))
