(cl:in-package #:asdf-user)

(defsystem "sicl-posix-high"
  :depends-on ("sicl-posix-low" "sicl-posix-high-package")
  :serial t
  :components
  ((:file "conditions")
   (:file "exit")
   (:file "read")
   (:file "write")))
