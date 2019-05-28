(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-phase-0
  :depends-on (#:sicl-hir-to-cl
               #:sicl-source-tracking
               #:sicl-data-and-control-flow-support
               #:eclector
               #:eclector-concrete-syntax-tree)
  :serial t
  :components
  ((:file "packages")
   (:file "import-from-host")
   (:file "define-defmacro")
   (:file "define-backquote-macro")
   (:file "define-default-setf-expander")
   (:file "define-setf-macro-function")
   (:file "fill-environment")
   (:file "environment")
   (:file "load-file")))
