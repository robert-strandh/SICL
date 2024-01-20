(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-shared"
  :depends-on ("common-boot"
               "common-boot-macros"
               "common-macro-definitions"
               "eclector-concrete-syntax-tree"
               "parcl-extrinsic"
               "parcl-class"
               "khazern-extrinsic"
               "sicl-environment"
               "sicl-source-tracking")
  :serial t
  :components
  ((:file "packages")
   (:file "cst-to-ast")
   (:file "load-source-file")
   (:file "asdf-programming")
   (:file "define-backquote-macros")))
