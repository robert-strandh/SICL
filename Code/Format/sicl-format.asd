(cl:in-package #:asdf-user)

(defsystem :sicl-format
  :depends-on (:cleavir-internationalization)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-en")
   (:file "format")))
