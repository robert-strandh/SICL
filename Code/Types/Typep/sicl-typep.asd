(cl:in-package #:asdf-user)

(defsystem "sicl-typep"
  :serial t
  :components
  ((:file "typep-atomic")
   (:file "typep-compound-integer")
   (:file "typep-compound")
   (:file "typep")))
