(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-base
  :depends-on (#:sicl-extrinsic-environment
               #:sicl-ast-to-hir
               #:sicl-hir-evaluator
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
   (:file "create-mop-classes")
   (:file "define-accessor-generic-functions")
   (:file "enable-generic-function-invocation")
   (:file "define-make-instance")
   (:file "enable-class-finalization")
   (:file "enable-defmethod")
   (:file "enable-object-initialization")
   (:file "enable-class-initialization")
   (:file "finalize-all-classes")
   (:file "header")
   (:file "define-class-of")
   (:file "enable-allocate-instance")
   (:file "compile-file")
   (:file "load-source")
   (:file "repl")))
