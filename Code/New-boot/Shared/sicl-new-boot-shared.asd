(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-shared"
  :depends-on ("common-boot-ast-interpreter"
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
   (:file "client")
   (:file "environment")
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
   (:file "define-ecclesia-functions")
   (:file "function-cell-interception")
   (:file "fill-environment")
   (:file "backtrace")
   (:file "repl")
   (:file "trace")))
