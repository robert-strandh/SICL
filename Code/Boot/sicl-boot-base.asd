(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-base
  :depends-on (#:sicl-extrinsic-environment
               #:sicl-ast-to-hir
               #:sicl-hir-interpreter
               #:sicl-hir-to-mir
               #:sicl-mir-to-lir
               #:sicl-code-generation
               #:sicl-code-object
               #:sicl-ast-compiler
               #:sicl-method-combination-support)
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "environment")
   (:file "boot-class")
   (:file "utilities")
   (:file "enable-defgeneric")
   (:file "enable-method-combinations")
   (:file "create-mop-classes")))
