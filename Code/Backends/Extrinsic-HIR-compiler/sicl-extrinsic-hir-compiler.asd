(cl:in-package #:asdf-user)

;;;; The purpose of this system is to create a SICL-like environment
;;;; executing inside the host Common Lisp implementation.  This goal
;;;; can be accomplished with different levels of ambition.  For this
;;;; particular system, we use all the data types from the host, and
;;;; of course the entire memory management functionality.
;;;;
;;;; There are some things that we HAVE TO import from the host, for
;;;; instance CONS, ALLOCATE-INSTANCE, and STANDARD-INSTANCE-ACCESS,
;;;; because they depend on how the basic data types are represented.
;;;; We must also import fundamental predicates such as CONSP,
;;;; SYMBOLP, etc.
;;;;
;;;; Similarly, there are things we absolution CAN NOT import from the
;;;; host.  This is true for macros that may expand to
;;;; implementation-specific code, and since it is too complicated to
;;;; determine for which macros this is true, we might as well not
;;;; import ANY host macros.  We also can not import functions that
;;;; work on the environment, directly or indirectly.  Functions such
;;;; as FDEFINITION and MACRO-FUNCTION are examples of functions that
;;;; directly manipulate the environment.  An example of a function
;;;; that indirectly manipulates the environment is
;;;; INITIALIZE-INSTANCE, in particular when an instance of a class is
;;;; initialized, because then an :AFTER method on INITIALIZE-INSTANCE
;;;; potentially creates a named generic function.
;;;;
;;;; Then, there is a gray area of stuff that can sometimes be taken
;;;; from the host.  Among other things, implementation-specific
;;;; acessors such as SYMBOL-NAME and PACKAGE-NICKNAMES fall into this
;;;; category.  On the one hand, in some sense, they must be taken
;;;; from the host because there is no other way of implementing these
;;;; functions.  On the other hand, they may FAIL because of some
;;;; argument mismatch.  We solve this problem by defining our own
;;;; versions of these functions, so that arguments will be checked,
;;;; but these versions then call the analogous host function to
;;;; accomplish its task.  It might not be practical to do this with
;;;; all functions that can fail, so there might be situations where
;;;; the host ERROR function is called rather than the SICL version of
;;;; it.
;;;;
;;;; The way this extrinsic environment is created is that we
;;;; initially import all functions from the host, and not only
;;;; functions with names in the COMMON-LISP package, but also our own
;;;; support functions.  Then we gradually replace these functions by
;;;; LOADing code into the environment.  This process translates the
;;;; code to HIR and then to a very low-level Common Lisp code that is
;;;; compiled by the host compiler.  Ultimately, most of the code that
;;;; was initially imported from the host is replaced by SICL code
;;;; that has been compiled with the SICL compiler.

(defsystem :sicl-extrinsic-hir-compiler
  :depends-on (:closer-mop
	       :sicl-conditionals-support
	       :sicl-loop-support
	       :sicl-arithmetic
	       :sicl-clos-package
	       :sicl-cons-package
	       :sicl-global-environment
	       :sicl-simple-environment
	       :sicl-environment
	       :sicl-evaluation-and-compilation
	       :sicl-data-and-control-flow-support
	       :eclector
	       :cleavir-hir
	       :cleavir-generate-ast
	       :cleavir-ast-to-hir
	       :sicl-extrinsic-environment
	       :cleavir-primop
	       :cleavir-hir-transformations
	       :cleavir-basic-blocks
	       :sicl-additional-conditions)
  :serial t
  :components
  ((:file "packages")
   (:file "host-cl-package")
   (:file "define-global-environment")
   (:file "create-global-environment")
   (:file "import-from-conditionals")
   (:file "import-from-loop")
   (:file "import-from-closer-mop")
   (:file "funcall")
   (:file "define-primops")
   (:file "fill-global-environment")
   (:file "load-file-defun")
   (:file "load-files")
   (:file "repl")))
