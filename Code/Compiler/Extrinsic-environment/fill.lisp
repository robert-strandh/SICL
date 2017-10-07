(cl:in-package #:sicl-extrinsic-environment)

(defun fill-environment (environment)
  (import-from-host environment)
  (import-from-sicl-global-environment environment)
  (setf (sicl-global-environment:fdefinition
	 'cleavir-primop:call-with-variable-bound
	 environment)
	(fdefinition 'call-with-variable-bound))
  (define-backquote-macros environment)
  (define-defmacro environment)
  (define-in-package environment)
  (define-default-setf-expander environment)
  ;; Load a file containing a definition of the macro LAMBDA.  This
  ;; macro is particularly simple, so it doesn't really matter how it
  ;; is expanded.  This is fortunate, because that the time this file
  ;; is loaded, the definition of DEFMACRO is still one we created
  ;; "manually" and which uses the host compiler to compile the macro
  ;; function in the null lexical environment.  We define the macro
  ;; LAMBDA before we redefine DEFMACRO as a target macro because
  ;; PARSE-MACRO returns a LAMBDA form, so we need this macro in order
  ;; to redefine DEFMACRO.
  (cst-load-file "../../Evaluation-and-compilation/lambda.lisp" environment)
  ;; Load a file containing the definition of the macro
  ;; MULTIPLE-VALUE-BIND.  We need it early because it is used in the
  ;; expansion of SETF, which we also need early for reasons explained
  ;; below.
  (cst-load-file "../../Environment/multiple-value-bind.lisp" environment)
  ;; Load a file containing a definition of the macro SETF. Recall
  ;; that, at this point, the definition of DEFMACRO is still the one
  ;; that we created "manually".  We need the SETF macro early,
  ;; because it is needed in order to define the macro DEFMACRO.  The
  ;; reason for that, is that the expansion of DEFMACRO uses SETF to
  ;; set the macro function.  We could have defined DEFMACRO to call
  ;; (SETF MACRO-FUNCTION) directly, but that would have been less
  ;; "natural", so we do it this way instead.
  (cst-load-file "../../Data-and-control-flow/setf.lisp" environment)
  ;; At this point, we have all the ingredients (the macros LAMBDA and
  ;; SETF) in order to redefine the macro DEFMACRO as a native macro.
  ;; SINCE we already have a primitive form of DEFMACRO, we use it to
  ;; define DEFMACRO.  The result of loading this file is that all new
  ;; macros defined subsequently will have their macro functions
  ;; compiled with the target compiler.  However, the macro function of
  ;; DEFMACRO is still compiled with the host compiler.
  (cst-load-file "../../Environment/defmacro-defmacro.lisp" environment)
  ;; As mentioned above, at this point, we have a version of DEFMACRO
  ;; that will compile the macro function of the macro definition using
  ;; the target compiler.  However, the macro function of the macro
  ;; DEFMACRO itself is still the result of using the host compiler.
  ;; By loading the definition of DEFMACRO again, we fix this
  ;; "problem".
  (cst-load-file "../../Environment/defmacro-defmacro.lisp" environment)
  ;; Now that have the final version of the macro DEFMACRO, we can
  ;; load the target version of the macro IN-PACKAGE.
  (cst-load-file "../../Environment/in-package.lisp" environment)
  ;; Up to this point, the macro function of the macro LAMBDA was
  ;; compiled using the host compiler.  Now that we have the final
  ;; version of the macro DEFMACRO, we can reload the file containing
  ;; the definition of the macro LAMBDA, which will cause the macro
  ;; function to be compiled with the target compiler.
  (cst-load-file "../../Evaluation-and-compilation/lambda.lisp" environment)
  ;; Load a file containing the definition of the macro
  ;; MULTIPLE-VALUE-LIST.  This definition is needed, because it is
  ;; used in the expansion of the macro NTH-VALUE loaded below.
  (cst-load-file "../../Data-and-control-flow/multiple-value-list.lisp" environment)
  ;; Load a file containing the definition of the macro NTH-VALUE.
  ;; This definition is needed by the function CONSTANTP which is
  ;; loaded as part of the file standard-environment-functions.lisp
  ;; loaded below.
  (cst-load-file "../../Data-and-control-flow/nth-value.lisp" environment)
  (cst-load-file "../../Data-and-control-flow/multiple-value-call-defmacro.lisp"
             environment)
  ;; Load a file containing the definition of macro DEFUN.
  (cst-load-file "../../Environment/defun-defmacro.lisp" environment)
  ;; Load file containing definition of function GET-SETF-EXPANSION.
  ;; We can not use the version of this function provided by the host,
  ;; because it takes an environment argument, and the host version
  ;; does not work with the Cleavir/SICL environment objects.
  (cst-load-file "../../Data-and-control-flow/get-setf-expansion.lisp" environment)
  ;; Load a file containing the definitions of the conditional macros
  ;; such as AND, OR, CASE, etc.
  (cst-load-file "../../Conditionals/macros.lisp" environment)
  ;; Load a file containing the definitions of the macros DEFVAR,
  ;; DEFPARAMETER, DEFCONSTANT, DEFTYPE, and DEFINE-COMPILER-MACRO.
  (cst-load-file "../../Environment/standard-environment-macros.lisp" environment)
  ;; Load a file containing the definition of the function
  ;; MACROEXPAND-1.
  (cst-load-file "../../Evaluation-and-compilation/macroexpand.lisp" environment)
  ;; Load a file containing the definitions of the functions
  ;; MACRO-FUNCTION and (SETF MACRO-FUNCTION).
  (cst-load-file "../../Evaluation-and-compilation/macro-function.lisp" environment)
  ;; Load a file containing the definition of the macro DECLAIM.
  (cst-load-file "../../Evaluation-and-compilation/declaim-defmacro.lisp" environment)
  ;; Load a file containing the definitions of the functions
  ;; FDEFINITION, (SETF FDEFINITION), SYMBOL-FUNCTION, etc.
  (cst-load-file "../../Environment/standard-environment-functions.lisp" environment)
  ;; Load a file containing the definitions of the macros INCF and DECF.
  (cst-load-file "../../Arithmetic/incf-decf-defmacro.lisp" environment)
  (cst-load-file "../../Loop/loop-defmacro.lisp" environment)
  ;; Load a file containing the definitions of the macros PUSH and POP.
  (cst-load-file "../../Cons/push-pop-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro RETURN.
  (cst-load-file "../../Data-and-control-flow/return-defmacro.lisp" environment)
  ;; Load a file containing the definitions of the macros PROG1 and PROG2.
  (cst-load-file "../../Data-and-control-flow/prog1-prog2-defmacro.lisp" environment)
  ;; Load a file containing the definitions of the macros PROG and PROG*.
  (cst-load-file "../../Data-and-control-flow/prog-progstar-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro PSETQ.
  (cst-load-file "../../Data-and-control-flow/psetq-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro PSETF.
  (cst-load-file "../../Data-and-control-flow/psetf-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro ROTATEF.
  (cst-load-file "../../Data-and-control-flow/rotatef-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro
  ;; DESTRUCTURING-BIND.
  (cst-load-file "../../Data-and-control-flow/destructuring-bind-defmacro.lisp"
	     environment)
  ;; Load a file containing the definition of the macro SHIFTF.
  (cst-load-file "../../Data-and-control-flow/shiftf-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro PUSHNEW.
  (cst-load-file "../../Cons/pushnew-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro DOTIMES.
  (cst-load-file "../../Iteration/dotimes-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro DOLIST.
  (cst-load-file "../../Iteration/dolist-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macros DO and DO*.
  ;; Temporarility comment this out because we don't use these macros
  ;; anyway.
  ;;  (cst-load-file "../../Iteration/do-dostar-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro
  ;; WITH-OPEN-STREAM.
  ;;  (cst-load-file "../../Stream/with-open-stream-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro WITH-SLOTS.
  (cst-load-file "../../CLOS/with-slots-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro WITH-ACCESSORS.
  (cst-load-file "../../CLOS/with-accessors-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro REMF.
  (cst-load-file "../../Cons/remf-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro DEFGENERIC.
  (cst-load-file "../../CLOS/defgeneric-defmacro.lisp" environment)
  (cst-load-file "../../Conditions/assert-defmacro.lisp" environment)
  ;; Load a file containing the definition of the macro DEFMETHOD.
  (cst-load-file "../../CLOS/defmethod-defmacro.lisp" environment)
  (cst-load-file "../../CLOS/defclass-support.lisp" environment)
  (cst-load-file "../../CLOS/defclass-defmacro.lisp" environment))
