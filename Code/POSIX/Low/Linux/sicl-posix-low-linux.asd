(cl:in-package #:asdf-user)

(defsystem #:sicl-posix-low-linux
  :depends-on (#:sicl-posix-low
               #:cluster
               #:cluster-x86-instruction-database)
  :components
  ((:file "error-codes")
   (:file "registers")
   (:file "utilities")
   (:file "exit")
   (:file "read")
   (:file "write")))
