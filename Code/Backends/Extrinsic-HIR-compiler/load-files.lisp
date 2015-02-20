(cl:in-package #:sicl-extrinsic-hir-compiler)

(defun load-files (environment)
  ;; Load a file containing a definition of the macro LAMBDA.  This
  ;; macro is particularly simple, so it doesn't really matter how it
  ;; is expanded.  This is fortunate, because that the time this file
  ;; is loaded, the definition of DEFMACRO is still one we created
  ;; "manually" and which uses the host compiler to compile the macro
  ;; function in the null lexical environment.  We define the macro
  ;; LAMBDA before we redefine DEFMACRO as a target macro because
  ;; PARSE-MACRO returns a LAMBDA form, so we need this macro in order
  ;; to redefine DEFMACRO.
  (load-file "../../Evaluation-and-compilation/lambda.lisp"
	     environment)

  ;; Load a file containing the definition of the macro
  ;; MULTIPLE-VALUE-BIND.  We need it early because it is used in the
  ;; expansion of SETF, which we also need early for reasons explained
  ;; below.
  (load-file "../../Environment/multiple-value-bind.lisp"
	     environment)

  ;; Load a file containing a definition of the macro SETF. Recall
  ;; that, at this point, the definition of DEFMACRO is still the one
  ;; that we created "manually".  We need the SETF macro early,
  ;; because it is needed in order to define the macro DEFMACRO.  The
  ;; reason for that, is that the expansion of DEFMACRO uses SETF to
  ;; set the macro function.  We could have defined DEFMACRO to call
  ;; (SETF MACRO-FUNCTION) directly, but that would have been less
  ;; "natural", so we do it this way instead.
  (load-file "../../Data-and-control-flow/setf.lisp"
	     environment)

  ;; At this point, we have all the ingredients (the macros LAMBDA and
  ;; SETF) in order to redefine the macro DEFMACRO as a native macro.
  ;; SINCE we already have a primitive form of DEFMACRO, we use it to
  ;; define DEFMACRO.  The result of loading this file is that all new
  ;; macros defined subsequently will have their macro functions
  ;; compiled with the target compiler.  However, the macro function
  ;; of DEFMACRO is still compiled with the host compiler.
  (load-file "../../Environment/defmacro-defmacro.lisp"
	     environment)

  ;; As mentioned above, at this point, we have a version of DEFMACRO
  ;; that will compile the macro function of the macro definition
  ;; using the target compiler.  However, the macro function of the
  ;; macro DEFMACRO itself is still the result of using the host
  ;; compiler.  By loading the definition of DEFMACRO again, we fix
  ;; this "problem".
  (load-file "../../Environment/defmacro-defmacro.lisp"
	     environment)

  ;; Now that have the final version of the macro DEFMACRO, we can
  ;; load the target version of the macro IN-PACKAGE.
  (load-file "../../Environment/in-package.lisp"
	     environment)

  ;; Up to this point, the macro function of the macro LAMBDA was
  ;; compiled using the host compiler.  Now that we have the final
  ;; version of the macro DEFMACRO, we can reload the file containing
  ;; the definition of the macro LAMBDA, which will cause the macro
  ;; function to be compiled with the target compiler.
  (load-file "../../Evaluation-and-compilation/lambda.lisp"
	     environment)

  ;; Load a file containing the definition of the macro
  ;; MULTIPLE-VALUE-LIST.  This definition is needed, because it is
  ;; used in the expansion of the macro NTH-VALUE loaded below.
  (load-file "../../Data-and-control-flow/multiple-value-list.lisp"
	     environment)

  ;; Load a file containing the definition of the macro NTH-VALUE.
  ;; This definition is needed by the function CONSTANTP which is
  ;; loaded as part of the file standard-environment-functions.lisp
  ;; loaded below.
  (load-file "../../Data-and-control-flow/nth-value.lisp"
	     environment)

  (load-file "../../Environment/defun-defmacro.lisp"
	     environment)
  (load-file "../../Conditionals/macros.lisp"
	     environment)
  (load-file "typep.lisp"
	     environment)
  (load-file "../../Environment/standard-environment-macros.lisp"
	     environment)
  (load-file "../../Environment/standard-environment-functions.lisp"
	     environment)
  (load-file "../../Data-and-control-flow/fboundp-defun.lisp"
	     environment)
  (load-file "../../Data-and-control-flow/fdefinition.lisp"
	     environment)
  (load-file "../../Data-and-control-flow/setf-fdefinition.lisp"
	     environment)
  (load-file "../../Data-and-control-flow/get-setf-expansion.lisp"
	     environment)
  (load-file "../../Data-and-control-flow/return-defmacro.lisp"
	     environment)
  (load-file "../../Cons/low.lisp"
	     environment)
  (load-file "../../Cons/cxr.lisp"
	     environment)
  (load-file "../../Cons/setf-cxr.lisp"
	     environment)
  (load-file "../../Loop/loop-defmacro.lisp"
	     environment)
  (load-file "../../Arithmetic/incf-decf-defmacro.lisp"
	     environment)

  ;; Load a file containing a temporary definition of the ordinary
  ;; function ENSURE-GENERIC-FUNCTION.  The real version of
  ;; ENSURE-GENERIC-FUNCTION calls ENSURE-GENERIC-FUNCTION-USING-CLASS
  ;; which is a generic function, but since we can't create generic
  ;; functions yet, we must break this dependency cycle somehow.
  (load-file "ensure-generic-function.lisp"
	     environment)

  ;; Load a file containing the definition of the macro DEFGENERIC.
  ;; That macro is particularly simple in that it expands to a call to
  ;; ensure-generic-function and it does not need any support code.
  (load-file "../../CLOS/defgeneric-defmacro.lisp"
	     environment)

  ;; Load a file containing the definition of the macro REMF. We load
  ;; it here because it is used in the function ENSURE-METHOD (see
  ;; below).
  (load-file "../../Cons/remf-defmacro.lisp"
	     environment)

  ;; Load a file containing the definition of function ENSURE-METHOD.
  ;; This function is not in the AMOP, but we use it in the expansion
  ;; of DEFMETHOD, so we need to define it here.
  (load-file "../../CLOS/ensure-method.lisp"
	     environment)

  ;; Load a file containing support code for MAKE-METHOD-LAMBDA.
  ;; Recall that MAKE-METHOD-LAMBDA is a generic function.  However,
  ;; we can not define generic functions or methods yet.  The support
  ;; code contains only ordinary functions that are called from the
  ;; methods of MAKE-METHOD-LAMBDA.  By structuring the code this way,
  ;; we can make a temporary version of MAKE-METHOD-LAMBDA as an
  ;; ordinary function that also calls the ordinary functions of the
  ;; support code.
  (load-file "../../CLOS/make-method-lambda-support.lisp"
	     environment)

  ;; Load a file containing a temporary definition of
  ;; MAKE-METHOD-LAMBDA as an ordinary function.  MAKE-METHOD-LAMBDA
  ;; is used by the macro DEFMETHOD, so we need some version of
  ;; MAKE-METHOD-LAMBDA before we can start using DEFMETHOD to define
  ;; methods.  Obviously, since we can not yet define methods, we can
  ;; not use the definitive version of MAKE-METHOD-LAMBDA which is a
  ;; generic function.  That is why we load this temporary version of
  ;; it.  Furthermore, we can not use the host version of
  ;; MAKE-METHOD-LAMBDA, because it is a "code generator", and as such
  ;; can very well generate code that is implementation-specific.
  (load-file "../../CLOS/make-method-lambda-defuns.lisp"
	     environment)

  ;; Load a file containing code used by the macro expander for the
  ;; macro DEFMETHOD.
  (load-file "../../CLOS/defmethod-support.lisp"
	     environment)

  (load-file "../../CLOS/defmethod-defmacro.lisp"
	     environment)

  (load-file "../../CLOS/ensure-generic-function-using-class-defgenerics.lisp"
	     environment)

  (load-file "../../CLOS/ensure-generic-function-using-class-support.lisp"
	     environment)

  (load-file "../../CLOS/ensure-generic-function-using-class-defmethods.lisp"
	     environment)

  ;; Load file containing final version of ENSURE-GENERIC-FUNCTION.
  ;; Since the generic function ENSURE-GENERIC-FUNCTION-USING-CLASS as
  ;; well as its specified methods exist, we can now replace the
  ;; temporary definition of this ordinary function by the final
  ;; version.
  (load-file "../../CLOS/ensure-generic-function.lisp"
	     environment))
