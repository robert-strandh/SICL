(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler
  :depends-on (:sicl-code-utilities
	       :sicl-compiler-utilities
	       :sicl-compiler-basic-blocks
	       :sicl-compiler-dominance
	       :sicl-compiler-liveness
	       :sicl-compiler-reaching-definitions
	       :sicl-compiler-def-use-chains
	       :sicl-compiler-ssa-form
	       :sicl-compiler-loops)
  :components
  ((:file "packages" :depends-on ())
   (:file "environment"
    :depends-on ("packages"))
   (:file "abstract-syntax-tree"
    :depends-on ("packages" "environment"))
   (:file "mir"
    :depends-on ("packages"))
   (:file "phase1"
    :depends-on ("packages" "environment" "abstract-syntax-tree"))
   (:file "phase2"
    :depends-on ("packages" "abstract-syntax-tree" "mir"))
   (:file "procedure-integration"
    :depends-on ("packages" "abstract-syntax-tree"))
   (:file "graph-coloring" :depends-on ("packages"))
   (:file "make"
    :depends-on ("packages"))
   (:file "program"
    :depends-on ("packages" "mir" "make"))))
