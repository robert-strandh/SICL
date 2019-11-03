(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-inspector
  :depends-on (#:sicl-boot
               #:clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "inspect")
   (:file "object-state-class")
   (:file "ersatz-instance-slot-place")
   (:file "present-impure-object-slots")
   (:file "impure-object")
   (:file "impure-standard-generic-function")
   (:file "impure-standard-class")
   (:file "impure-funcallable-standard-class")
   (:file "impure-built-in-class")
   (:file "impure-standard-method")
   (:file "impure-standard-reader-method")
   (:file "impure-standard-writer-method")
   (:file "impure-direct-slot-definition")
   (:file "impure-effective-slot-definition")
   (:file "present-pure-object-slots")
   (:file "pure-object")
   (:file "pure-standard-generic-function")))
