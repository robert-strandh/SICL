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

(defsystem sicl-extrinsic-environment
  :depends-on (:concrete-syntax-tree
               :trivial-gray-streams
               :sicl-simple-environment
               :sicl-loop-support
               :sicl-arithmetic
               :sicl-cons-support
               :sicl-clos-support
               :sicl-type-support
               :sicl-conditions-support
               :sicl-environment-support
               :closer-mop
               :cleavir2-cst-to-ast
               :cleavir-ast-transformations
               :cleavir-meter
               :sicl-evaluation-and-compilation-support
               :sicl-data-and-control-flow-support
               :sicl-conditionals-support
               :sicl-iteration-support
               :sicl-source-tracking
               :eclector
               :eclector-concrete-syntax-tree)
  :serial t
  :components
  ((:file "packages")
   (:file "host-load")
   (:file "environment")
   (:file "import-from-host")))
