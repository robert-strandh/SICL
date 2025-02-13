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
   (:file "set-static-environment-instruction")
   (:file "static-environment-instruction")
   (:file "dynamic-environment-instruction")
   (:file "read-static-environment-instruction")
   (:file "if-instruction")
   (:file "exit-point-instruction")
   (:file "unwind-instruction")
   (:file "funcall-instruction")
   (:file "return-instruction")
   (:file "assignment-instruction")))
