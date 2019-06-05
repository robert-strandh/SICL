(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-phase-0
  :depends-on (#:sicl-new-boot-base
               #:sicl-hir-to-cl
               #:sicl-source-tracking
               #:sicl-data-and-control-flow-support
               #:sicl-package-support
               #:sicl-sequence-support
               #:sicl-arithmetic
               #:sicl-method-combination-support
               #:eclector
               #:eclector-concrete-syntax-tree
               #:cleavir-io)
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "eval")
   (:file "compile-file")
   (:file "host-load")
   (:file "import-from-host")
   (:file "define-defmacro")
   (:file "define-backquote-macro")
   (:file "define-default-setf-expander")
   (:file "define-setf-macro-function")
   (:file "fill-environment")
   (:file "environment")
   (:file "load-file")
   (:file "compile-files")
   (:file "boot")))
