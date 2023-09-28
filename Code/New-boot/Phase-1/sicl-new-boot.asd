(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot"
  :depends-on ("common-boot"
               "common-boot-macros"
               "common-macro-definitions")
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "configuration")
   (:file "import-from-host")
   (:file "define-environment-functions")
   (:file "cst-to-ast")))
