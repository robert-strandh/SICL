(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-8
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "load-arithmetic-functions")
   (:file "load-hash-table-functionality")
   (:file "boot")))
