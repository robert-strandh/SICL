(cl:in-package #:asdf-user)

(defsystem #:sicl-register-allocation
  :serial t
  :components
  ((:file "packages")
   (:file "location-information")
   (:file "find-back-arcs")
   (:file "pool")
   (:file "work-list")
   (:file "estimated-distance-to-use")
   (:file "registers")
   (:file "arrangement")
   (:file "spill-unspill")
   (:file "allocate-register")))
