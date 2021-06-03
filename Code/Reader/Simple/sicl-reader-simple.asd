(cl:in-package #:asdf-user)

(defsystem :sicl-reader-simple
  :depends-on (:cleavir-code-utilities
               :concrete-syntax-tree
               :sicl-simple-readtable
               :sicl-host-mop)
  :serial t
  :components
  ((:file "packages")
   (:file "more-variables")
   (:file "additional-conditions")
   (:file "utilities")
   (:file "tokens")
   (:file "read-common")
   (:file "read")
   (:file "macro-functions")
   (:file "init")
   (:file "quasiquote-macro")
   (:file "fixup")))
