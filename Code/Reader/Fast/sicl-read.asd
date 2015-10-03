(cl:in-package #:asdf-user)

(defsystem :sicl-read
  :depends-on (:cleavir-internationalization)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "float")
   (:file "read")))
