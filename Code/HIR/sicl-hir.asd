(cl:in-package #:asdf-user)

(defsystem "sicl-hir"
  :depends-on ()
  :serial t
  :components
  ((:file "packages")
   (:file "instruction")
   (:file "data")
   (:file "parse-arguments-instruction")
   (:file "make-cell-instruction")
   (:file "read-cell-instruction")
   (:file "write-cell-instruction")
   (:file "write-static-environment-instruction")
   (:file "static-environment-instruction")))
