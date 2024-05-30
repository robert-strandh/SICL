(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-shared"
  :depends-on ("common-boot"
               "common-boot-macros"
               "common-macro-definitions"
               "sicl-environment-extrinsic"
               "eclector-concrete-syntax-tree"
               "parcl-low"
               "parcl-extrinsic"
               "parcl-low-class"
               "khazern-extrinsic"
               "sicl-source-tracking")
  :serial t
  :components
  ((:file "packages")
   (:file "boot-class")
   (:file "environment")
   (:file "client")
   (:file "package-programming")
   (:file "cst-to-ast")
   (:file "load-source-file")
   (:file "asdf-programming")
   (:file "define-backquote-macros")
   (:file "import-from-host")
   (:file "read-symbol-components")
   (:file "define-environment-functions")
   (:file "import-khazern")
   (:file "ersatz-object")
   (:file "define-clostrophilia-find-method-combination-template")
   (:file "define-client-and-environment-variables")
   (:file "define-straddle-functions")
   (:file "repl")))
