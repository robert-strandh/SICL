(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-phase-1"
  :depends-on ("common-boot"
               "common-boot-macros"
               "common-macro-definitions"
               "eclector-concrete-syntax-tree")
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "configuration")
   (:file "import-from-host")
   (:file "define-environment-functions")
   (:file "cst-to-ast")
   (:file "load-file")))
