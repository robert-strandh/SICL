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
	       :sicl-compiler-loops
	       :sicl-environment
	       :sicl-reader-simple
	       :sicl-type)
  :components
  ((:file "packages" :depends-on ())
   (:file "abstract-syntax-tree"
    :depends-on ("packages"))
   (:file "mir"
    :depends-on ("packages"))
   (:file "phase1"
    :depends-on ("packages" "abstract-syntax-tree"))
   (:file "ast-transformations"
    :depends-on ("packages"))
   (:file "phase2"
    :depends-on ("packages" "abstract-syntax-tree" "mir"))
   (:file "graph-coloring" :depends-on ("packages"))
   (:file "make"
    :depends-on ("packages"))
   (:file "program"
    :depends-on ("packages" "mir" "make"))
   (:file "type-map"
    :depends-on ("packages"))
   (:file "type-inference"
    :depends-on ("packages" "type-map" "mir"))
   (:file "compile-file"
    :depends-on ("program"
		 "ast-transformations"
		 "type-map"
		 "graph-coloring"
		 "phase1"
		 "phase2"))
   (:file "compile-time-compile"
    :depends-on ("phase1"))))
