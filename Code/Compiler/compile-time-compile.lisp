(in-package #:sicl-compiler)

(defun compile-time-compile (name lambda-expression)
  (let ((lambda-list (cadr lambda-expression)))
    (if (null (intersection lambda-list lambda-list-keywords))
	;; FIXME: Right now we always save the AST, but we should
	;; probably save it only when the function is currently
	;; declared INLINE.
	(multiple-value-bind (lexical-asts ast)
	    (sicl-compiler-phase-1:convert-for-inlining lambda-expression)
	  (sicl-env:ensure-global-function-entry
	   name lambda-list ast lexical-asts))
	  (sicl-env:ensure-global-function-entry
	   name lambda-list nil nil))))

