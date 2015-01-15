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

(load (rp "../../Environment/multiple-value-bind.lisp"))
(load (rp "../../Data-and-control-flow/setf.lisp"))
(load (rp "defmacro-defmacro.lisp"))
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
