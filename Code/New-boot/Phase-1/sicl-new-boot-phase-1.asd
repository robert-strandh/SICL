(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-phase-1"
  :depends-on ("common-boot"
               "common-boot-macros"
               "common-macro-definitions"
               "eclector-concrete-syntax-tree"
               "parcl-extrinsic"
               "parcl-class"
               "khazern-extrinsic")
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "environment")
   (:file "configuration")
   (:file "macro-programming")
   (:file "package-programming")
   (:file "import-from-host")
   (:file "import-khazern")
   (:file "define-environment-functions")
   (:file "cst-to-ast")
   (:file "load-file")
   (:file "boot")))
