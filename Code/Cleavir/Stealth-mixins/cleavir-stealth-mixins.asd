(cl:in-package #:asdf-user)

(defsystem :cleavir-stealth-mixins
  :depends-on (:closer-mop)
  :serial t
  :components
  ((:file "packages")
   (:file "stealth-mixins")))
