(cl:in-package #:asdf-user)

(defsystem #:sicl-register-allocation
  :serial t
  :components
  ((:file "packages")
   (:file "location-information")
   (:file "find-back-arcs")
   (:file "estimated-distance-to-use")))
