(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-shared"
  :serial t
  :components
  ((:file "packages")
   (:file "cst-to-ast")
   (:file "load-source-file")
   (:file "asdf-programming")
   (:file "define-backquote-macros")))

