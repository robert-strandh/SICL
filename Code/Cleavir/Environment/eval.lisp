(cl:in-package #:cleavir-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function EVAL.
;;;
;;; We define EVAL as a generic function with three arguments: a form
;;; and two environments.  The first environment is the environment in
;;; which the form is to be evaluated.  The second environment is used
;;; for generic dispatch only.
;;;
;;; When some Cleavir tools such as the minimal compiler or the AST
;;; generator are used in a context where a top-level form is
;;; processed by the file compiler, it handles EVAL-WHEN with a
;;; situation of :COMPILE-TOPLEVEL by calling the EVAL generic
;;; function.
;;;
;;; For the discussion that follows, recall that we use the word
;;; "intrinsic" to mean "native", as in the host and the target
;;; environment are the same, and we use the word "extrinsic" to mean
;;; "cross", i.e., the two environments are not the same.
;;;
;;; The reason for calling the Cleavir-specific generic function
;;; rather than just calling CL:EVAL is that CL:EVAL might not be the
;;; right thing to do.
;;;
;;; One problematic case is when the global environment (the so-called
;;; evaluation environment) is side effected at compile time, for
;;; instance as a result of evaluating a DEFUN form at compile time.
;;;
;;;   * When the Cleavir tools in question are used as extrinsic tools
;;;     (i.e., the host system is different from the target system),
;;;     then CL:EVAL would side effect the host environment, whereas
;;;     it is more likely that the target environment should be side
;;;     effected.  For instance, when a target system is bootstrapped
;;;     on a host system, the Cleavir tools might be used to fill up
;;;     the target environment with definitions of standard macros and
;;;     special operators.
;;;
;;;   * A solution that always works would be to handle compile-time
;;;     evaluation by converting the form to an AST and then
;;;     interpreting that AST.  However, then we have the inverse
;;;     problem, namely when the tools are used as intrinsic (native)
;;;     tools, then we might store interpreted functions in the global
;;;     environment.  So, although the solution works, it might result
;;;     in slow code.  In this case, it is more likely that we want to
;;;     call the native EVAL function in order to obtain a compiled
;;;     function.
;;;
;;; Another problematic case with compile-time evaluation is that the
;;; standard treats the body of a LOCALLY, MACROLET, or a
;;; SYMBOL-MACROLET as a top-level form when the FORM itself is a
;;; top-level form.  The HyperSpec has a remark saying "Note that this
;;; implies that the lexical environment in which top level forms are
;;; processed is not necessarily the null lexical environment".  The
;;; problem here arises when the tools are used as intrinsic tools
;;; (i.e., the host and the target environments are the same).  We
;;; can't just call CL:EVAL, because CL:EVAL always evaluates the form
;;; in the null lexical environment.
;;;
;;; Finally, the generic EVAL provides a solution that Cleavir uses
;;; for processing MACROLET forms.  The problem here is that the local
;;; macro definitions should be processed differently in the extrinsic
;;; and the intrinsic case.  In the extrinsic case, the expander
;;; functions should be generated as host functions so that expansion
;;; can take place at compile time.  Cleavir does this by calling the
;;; generic EVAL to process the lambda expressions resulting from
;;; macro definitions.
;;;
;;; When the Cleavir tools are used as extrinsic tools, a reasonable
;;; method on EVAL would be to convert the form to an AST and then
;;; interpreting that AST in the global environment.  It is
;;; reasonable, because the AST presumably already contains all the
;;; results of the influence of the environment on the compilation
;;; process.  Furthermore, any side effect on the global environment
;;; will then happen in the target environment.  Function definitions
;;; that are entered into the target environment are interpreted, of
;;; course, but that is the only option for the extrinsic case anyway.
;;;
;;; For the intrinsic case, the right solution is to convert the form
;;; to an AST using the first environment and then to native code, and
;;; finally executing that native code.  This solution ensures that
;;; function definitions create native code.
;;;
;;; A quick-and-dirty (but incorrect) solution for the intrinsic case
;;; might be to minimally compile (in the sense of "minimal
;;; compilation") the form and then calling CL:EVAL on the result.  It
;;; is incorrect because any declarations present in the environment
;;; are lost when CL:EVAL is called.

(defgeneric eval (form environment dispatch-environment))

(defmethod eval (form environment1 (environment2 entry))
  (eval form environment1 (next environment2)))

;;; This version of EVAL takes a concrete syntax tree rather than a
;;; Common Lisp form.
(defgeneric cst-eval (cst environment dispatch-environment))

(defmethod cst-eval (cst environment1 (environment2 entry))
  (cst-eval cst environment1 (next environment2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MACRO-FUNCTION.
;;;
;;; We define MACRO-FUNCTION as a generic function with two arguments:
;;; a symbol and an environment.
;;;
;;; An implementation can use this function in the following way.
;;; Rather than defining implementation-specific methods on
;;; environment augmentation functions that create instances of
;;; corresponding implementation-specific environment augmentation
;;; classes, the implementation might choose to handle both the
;;; implementation-specific augmentation classes and the default
;;; augmentation classes provided by Cleavir.  In order to make this
;;; technique work, the implementation needs to do two things:
;;;
;;;   * Modify the existing implementation of CL:MACRO-FUNCTION so
;;;     that it calls the generic MACRO-FUNCTION here.  If the
;;;     optional argument to CL:MACRO-FUNCTION was supplied, it is
;;;     passed directly as the second argument to the generic
;;;     MACRO-FUNCTION.  If not, an argument representing the global
;;;     environment is passed instead.
;;;
;;;   * Supply one or more methods on the generic MACRO-FUNCTION,
;;;     specialized to the implementation-specific global environment
;;;     classes (for implementations with first-class global
;;;     environments), or to an artificial class used as a proxy for
;;;     the global environment (for implementations that do not have
;;;     first-class global environments).  These methods should return
;;;     NIL when no macro function is found.
;;;
;;;   * Also, if CL:MACRO-FUNCTION will still be called with
;;;     implementation-specific lexical environments, supply one or
;;;     more methods on the generic MACRO-FUNCTION specialized to the
;;;     implementations-specific lexical environment classes.

(defgeneric macro-function (symbol environment))

;;; The default method specialized to ENTRY is called for entries that
;;; are not of type MACRO.  This method just makes a recursive call,
;;; passing the next environment as an argument.
(defmethod macro-function (symbol (environment entry))
  (macro-function symbol (next environment)))

;;; This method is invoked when the environment is of type MACRO so it
;;; might potentially contain the macro function that we are looking
;;; for.  It is the one we are looking for if and only if the NAME of
;;; the environment is the symbol we are passed as an argument.  If
;;; not, we continue searching in the next environment.
(defmethod macro-function (symbol (environment macro))
  (if (eq symbol (name environment))
      (expander environment)
      (macro-function symbol (next environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPILER-MACRO-FUNCTION
;;;
;;; The standard Common Lisp function MACRO-FUNCTION takes an optional
;;; ENVIRONMENT argument.  When that argument is a local environment
;;; in which there is a local function or macro definition with the
;;; same name, then that local definition shadows any existing global
;;; macro function.  For that reason, we must first search the local
;;; environment and return NIL if we see a local function or macro
;;; definition.  We accomplish this by defining a generic function,
;;; also called COMPILER-MACRO-FUNCTION.  We supply methods on that
;;; generic function that return NIL when called with a local function
;;; or macro definition with the right name.
;;;
;;; Implementations should define a method on this generic function
;;; that accomplishes the task for the specific global environment.

(defgeneric compiler-macro-function (function-name environment))

;;; The default method specialized to ENTRY is called for entries that
;;; are not of type MACRO or FUNCTION.  This method just makes a
;;; recursive call, passing the next environment as an argument.
(defmethod compiler-macro-function (symbol (environment entry))
  (macro-function symbol (next environment)))

;;; This method is invoked when the environment is of type MACRO so it
;;; might potentially contain a macro function with the same name as
;;; the compiler macro.  If it does, we return NIL.  If not, we
;;; continue searching in the next environment.
(defmethod compiler-macro-function (function-name (environment macro))
  (if (eq function-name (name environment))
      nil
      (compiler-macro-function function-name (next environment))))

;;; This method is invoked when the environment is of type FUNCTION so
;;; it might potentially contain a function with the same name as the
;;; compiler macro.  If it does, we return NIL.  If not, we continue
;;; searching in the next environment.
(defmethod compiler-macro-function (function-name (environment function))
  (if (eq function-name (name environment))
      nil
      (compiler-macro-function function-name (next environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SYMBOL-MACRO-EXPANSION.

(defgeneric symbol-macro-expansion (symbol environment))

(defmethod symbol-macro-expansion (symbol (environment entry))
  (symbol-macro-expansion symbol (next environment)))

(defmethod symbol-macro-expansion (symbol (environment symbol-macro))
  (if (eq symbol (name environment))
      (expansion environment)
      (symbol-macro-expansion symbol (next environment))))
