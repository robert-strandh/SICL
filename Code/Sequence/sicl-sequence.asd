(cl:in-package #:asdf-user)

(defsystem :sicl-sequence
  :serial t
  :depends-on ("acclimation" "sicl-utilities")
  :components
  ((:module "Generic"
    :components
    ((:file "packages")
     (:file "Implementation-specific/sicl" :if-feature :sicl)
     (:file "Implementation-specific/default" :if-feature (:not :sicl))
     (:file "utilities")
     (:file "conditions")
     (:file "condition-reporters-en")
     (:file "for-each-relevant-cons")
     (:file "for-each-relevant-element")
     (:file "generic-functions")
     (:file "length")
     (:file "find")))))

