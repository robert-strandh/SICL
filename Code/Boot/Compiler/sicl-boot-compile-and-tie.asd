(cl:in-package #:asdf-user)

;;; This system is used to compile and then tie code at bootstrapping
;;; time.  It is meant to be executed by a host Common Lisp system
;;; during bootstrapping.  The translation from source code to native
;;; instructions is done the same way, and by the same code, as in the
;;; native compiler.  But the "tie" part is different.  In the native
;;; system, tying is done by the native top-level instructions
;;; resulting from the translation of a compilation unit to native
;;; instructions.  In this system, tying is done by the top-level
;;; function resulting from the translation of the HIR code to HIR
;;; thunks, done by the HIR evaluator.

(defsystem #:sicl-boot-compile-and-tie
  :depends-on (#:sicl-ast-to-hir
               #:sicl-hir-transformations
               #:sicl-hir-evaluator
               #:sicl-hir-to-mir
               #:sicl-mir-to-lir
               #:sicl-code-generation
               #:cluster
               #:cluster-x86-instruction-database
               #:sicl-code-object)
  :serial t
  :components
  ((:file "packages")))
