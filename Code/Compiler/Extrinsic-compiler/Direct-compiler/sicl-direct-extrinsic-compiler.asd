(cl:in-package #:common-lisp-user)

;;;; The direct extrinsic compiler is a compiler that runs on some
;;;; host environment perhaps other than SICL.  It supplies a function
;;;; that compiles a single lambda expression in the null lexical
;;;; environment, and it produces a small graph of objects that can be
;;;; turned into target binary code in a straightforward way.

(asdf:defsystem :sicl-direct-extrinsic-compiler
  :depends-on (:sicl-extrinsic-environment
	       :cleavir-generate-ast
	       :cleavir-ast-to-hir
	       :sicl-hir-to-mir
	       :cleavir-hir-transformations
	       :cleavir-liveness
	       :cleavir-register-allocation)
  :serial t
  :components
  ((:file "packages")
   (:file "classes")
   (:file "introduce-immediates")
   (:file "process-constants")
   (:file "compile")))
