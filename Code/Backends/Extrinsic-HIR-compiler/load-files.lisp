(cl:in-package #:sicl-extrinsic-hir-compiler)

(defun rp (filename)
  (asdf:system-relative-pathname :sicl-extrinsic-hir-compiler filename))

;;; Load a file containing a definition of the macro LAMBDA.  This
;;; macro is particularly simple, so it doesn't really matter how it
;;; is expanded.  This is fortunate, because that the time this file
;;; is loaded, the definition of DEFMACRO is still one we created
;;; "manually" and which uses the host compiler to compile the macro
;;; function in the null lexical environment.  We define the macro
;;; LAMBDA before we redefine DEFMACRO as a target macro because
;;; PARSE-MACRO returns a LAMBDA form, so we need this macro in order
;;; to redefine DEFMACRO.
(load (rp "../../Evaluation-and-compilation/lambda.lisp"))

;;; Load a file containing the definition of the macro
;;; MULTIPLE-VALUE-BIND.  We need it early because it is used in the
;;; expansion of SETF, which we also need early for reasons explained
;;; below.
(load (rp "../../Environment/multiple-value-bind.lisp"))

;;; Load a file containing a definition of the macro SETF. Recall
;;; that, at this point, the definition of DEFMACRO is still the one
;;; that we created "manually".  We need the SETF macro early, because
;;; it is needed in order to define the macro DEFMACRO.  The reason
;;; for that, is that the expansion of DEFMACRO uses SETF to set the
;;; macro function.  We could have defined DEFMACRO to call (SETF
;;; MACRO-FUNCTION) directly, but that would have been less "natural",
;;; so we do it this way instead.
(load (rp "../../Data-and-control-flow/setf.lisp"))

;;; At this point, we have all the ingredients (the macros LAMBDA and
;;; SETF) in order to redefine the macro DEFMACRO as a native macro.
;;; SINCE we already have a primitive form of DEFMACRO, we use it to
;;; define DEFMACRO.  The result of loading this file is that all new
;;; macros defined subsequently will have their macro functions
;;; compiled with the target compiler.  However, the macro function of
;;; DEFMACRO is still compiled with the host compiler.
(load (rp "defmacro-defmacro.lisp"))

;;; Load a file containing the definition of the macro
;;; MULTIPLE-VALUE-LIST.  This definition is needed, because it is
;;; used in the expansion of the macro NTH-VALUE loaded below.
(load (rp "../../Data-and-control-flow/multiple-value-list.lisp"))

(load (rp "../../Data-and-control-flow/nth-value.lisp"))
(load (rp "../../Environment/defun.lisp"))
(load (rp "../../Conditionals/macros.lisp"))
(load (rp "typep.lisp"))
(load (rp "../../Environment/standard-environment-macros.lisp"))
(load (rp "../../Environment/standard-environment-functions.lisp"))
(load (rp "../../Data-and-control-flow/fboundp.lisp"))
(load (rp "../../Data-and-control-flow/fdefinition.lisp"))
(load (rp "../../Data-and-control-flow/setf-fdefinition.lisp"))
(load (rp "../../Data-and-control-flow/get-setf-expansion.lisp"))
(load (rp "../../Cons/low.lisp"))
(load (rp "../../Cons/high.lisp"))
(load (rp "../../Loop/loop-defmacro.lisp"))
(load (rp "../../Arithmetic/incf-decf-defmacro.lisp"))
(load (rp "ensure-generic-function.lisp"))
