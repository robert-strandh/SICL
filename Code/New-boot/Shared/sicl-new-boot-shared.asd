(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-shared"
  :depends-on ("common-boot"
               "common-boot-macros"
               "common-macro-definitions"
               "sicl-environment-extrinsic"
               "eclector-concrete-syntax-tree"
               "parcl-extrinsic"
               "parcl-class"
               "khazern-extrinsic"
               "sicl-environment"
               "sicl-source-tracking")
  :serial t
  :components
  ((:file "packages")
   (:file "boot-class")
   (:file "client")
   (:file "package-programming")
   (:file "cst-to-ast")
   (:file "load-source-file")
   (:file "asdf-programming")
   (:file "define-backquote-macros")
   (:file "import-from-host")
   (:file "define-environment-functions")
   (:file "import-khazern")))
