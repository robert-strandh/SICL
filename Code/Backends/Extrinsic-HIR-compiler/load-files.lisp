(cl:in-package #:sicl-extrinsic-hir-compiler)

(defun load-files (environment)
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
