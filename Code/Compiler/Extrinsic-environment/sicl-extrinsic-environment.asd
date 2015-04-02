(cl:in-package #:asdf-user)

;;;; This system makes it possible to instantiate an EXTRINSIC
;;;; ENVIRONMENT.
;;;;
;;;; An extrinsic environment is a SICL first-class global environment
;;;; instantiated as a host object in a host system.  The main purpose
;;;; of such an environment is to serve as the compilation environment
;;;; for extrinsic compilation.  The assumption here is that READ
;;;; produces forms made up entirely of host objects.  The extrinsic
;;;; compiler must take such forms and produce abstract syntax trees
;;;; according to the contents of the environment.  Extrinsic
;;;; compilation might require an environment in later compilation
;;;; phases, but that environment could then be distinct from the
;;;; compilation environment we are talking about here.
;;;;
;;;; Perhaps the most important extrinsic compiler is the extrinsic
;;;; FILE COMPILER, often called the CROSS COMPILER.  In this
;;;; compiler, processing beyond creating an abstract syntax tree
;;;; consists of generating target code in a FASL file.  The FASL file
;;;; is then loaded into the native target system.
;;;;
;;;; As it is, this environment is only designed to handle the usual
;;;; top-level defining forms such as DEFUN, DEFGENERIC, DEFMETHOD,
;;;; DEFCLASS, DEFVAR, DEFPARAMETER, and DEFCONSTANT.  It can of
;;;; course evaluate any top-level form at compile time, but since
;;;; most functions in the environment come from the host, they may
;;;; not have the desired effect.  For instance, an attempt to call
;;;; (SETF CL:FDEFINITION) will set the function definition in the
;;;; host environment, rather than in this environment.  Client code
;;;; that needs to evaluate more sophisticated top-level forms must
;;;; first evaluate forms in this environment that create the correct
;;;; function definitions that are needed.  It is not hard to do that,
;;;; of course.  Basically, it suffices to load a file containing such
;;;; function definitions into this environment.

;;;; Extrinsic compilation imposes some non-trivial requirements on
;;;; the extrinsic environment:
;;;;
;;;;   * Clearly, we want the macros in the extrinsic environment to
;;;;     be target macros.  Host macros frequently expand into
;;;;     host-specific code which would make the resulting AST contain
;;;;     references to host functions, which is contrary to the
;;;;     purpose of extrinsic compilation.
;;;;
;;;;   * Macro expansion requires macro functions stored in the
;;;;     extrinsic environment to be executable by the extrinsic
;;;;     compiler.  There are several options possible here, including
;;;;     interpretation, byte-compilation, etc.  The option chosen
;;;;     here is to use ordinary host functions as macro functions.
;;;;
;;;;   * In many situations, the extrinsic compiler must be able to
;;;;     create functions in the compilation environment.  One such
;;;;     situation is when the special operator MACROLET is used in
;;;;     the source code to be compiled.  Since we are using ordinary
;;;;     host functions as macro functions, it might seem possible to
;;;;     just use the ordinary host compiler (COMPILE) to create a
;;;;     macro functions, but that solution does not work very well.
;;;;     The reason is that macros may very well be defined in a
;;;;     non-null lexical environment.  We solve this problem by
;;;;     recursively calling the extrinsic compiler in order to
;;;;     produce High-level Intermediate Representation (HIR) from the
;;;;     abstract syntax tree.  The HIR code is then translated to
;;;;     host code that refers to the extrinsic lexical environment.
;;;;
;;;;   * In the most general case, a form to be compiled may contain
;;;;     an EVAL-WHEN special form, requiring compile-time evaluation
;;;;     of some arbitrary form.  We accomplish this evaluation in the
;;;;     same way, i.e. we create HIR code from the form to evaluate,
;;;;     turn the HIR into a host function, and finally we execute the
;;;;     resulting function.
;;;;
;;;; Since it must be possible to define and execute functions at
;;;; compile time, the extrinsic environment must also supply a
;;;; DYNAMIC RUN-TIME ENVIRONMENT.  There are several reasons for that:
;;;;
;;;;   * It is possible that a function is defined and then executed
;;;;     at compile time and that function uses the dynamic non-local
;;;;     exist mechanisms CATCH, THROW, and UNWIND-PROTECT.
;;;;
;;;;   * It is possible that a function is defined and then executed
;;;;     at compile time and that function refers to and/or binds a
;;;;     special variable.
;;;;
;;;; We solve this problem by defining an explicit heap-allocated
;;;; dynamic run-time environment in the form of a list of entries for
;;;; those special operators.  When unwinding the run-time stack is
;;;; required, the dynamic run-time environment contains THUNKS that,
;;;; when executed, execute a host lexical non-local exit in the form
;;;; of a host GO form.  For the special-variable mechanism, an entry
;;;; in the dynamic run-time environment is provided for this purpose.
;;;;
;;;; Luckily, the entire condition mechanism can be defined on top of
;;;; the special variable binding mechanism.  The extrinsic
;;;; environment therefore does not have to supply the condition
;;;; system as an explicit mechanism.  Client code that needs this
;;;; functionality must provide it.

(defsystem :sicl-extrinsic-environment
  :depends-on (:sicl-simple-environment
	       :sicl-environment
	       :sicl-loop-support
	       :sicl-arithmetic
	       :sicl-cons-support
	       :sicl-clos-support
	       :closer-mop
	       :cleavir-generate-ast
	       :cleavir-ast-transformations
	       :cleavir-ast-to-hir
	       :cleavir-hir
	       :cleavir-hir-transformations
	       :cleavir-basic-blocks
	       :cleavir-meter
	       :sicl-evaluation-and-compilation
	       :sicl-data-and-control-flow
	       :sicl-conditionals-support
	       :sicl-iteration-support
	       :sicl-reader-simple)
  :serial t
  :components
  ((:file "packages")
   (:file "runtime-environment")
   (:file "symbol-value")
   (:file "traced-funcall")
   (:file "parse-arguments")
   (:file "translate-hir")
   (:file "environment-defclass")
   (:file "customization")
   (:file "eval")
   (:file "repl")
   (:file "load")
   (:file "load-file-defun")
   (:file "import-from-host")
   (:file "import-from-sicl-global-environment")
   (:file "define-defmacro")
   (:file "define-in-package")
   (:file "define-default-setf-expander")
   (:file "define-backquote-macros")
   (:file "fill")
   (:file "initialization")))
