(cl:in-package #:cleavir-environment)

;;;; This file defines a function COMPILE-TIME that reduces an ENTRY
;;;; to only those components that are "fully available" during
;;;; compilation. This is just as for MACROLET:
;;;; "The macro-expansion functions defined by macrolet are defined
;;;;  in the lexical environment in which the macrolet form appears.
;;;;  Declarations and macrolet and symbol-macrolet definitions affect
;;;;  the local macro definitions in a macrolet, but the consequences
;;;;  are undefined if the local macro definitions reference any local
;;;;  variable or function bindings that are visible in that lexical
;;;;  environment."
;;;; Therefore this function strips all lexical variables,
;;;;  functions, etc, while leaving macros and declarations.
;;;; This makes CLEAVIR-ENV:EVAL easier: it can be
;;;; (funcall (cleavir-compile form env)) where cleavir-compile is
;;;; in terms of GENERATE-AST etc.
;;;; If this was done with an unstripped environment, the AST/HIR/etc.
;;;; could refer to variables/functions that are not actually defined,
;;;; which could result in bad memory reads by a trusting compiler.

;;; We don't want to keep e.g. LEXICAL-VARIABLE environments, but we do need
;;; to know they're there, because they override earlier bindings.
;;; For example, in the following BAR refers to the local function and is
;;; therefore nonconformant.
;;; (macrolet ((foo ())) (flet ((foo ())) (macrolet ((bar () (foo))))))
;;; So we keep lists of shadowed names that we don't want environments for.
(defgeneric compile-time (environment &key variable-shadow function-shadow))

;;; Default, for the global environment: just return it.
(defmethod compile-time (environment &key variable-shadow function-shadow)
  (declare (ignore variable-shadow function-shadow))
  environment)

;;; If we don't specifically preserve an entry, lose it. (e.g. BLOCK)
(defmethod compile-time ((environment entry) &key variable-shadow function-shadow)
  (compile-time (next environment)
    :variable-shadow variable-shadow :function-shadow function-shadow))

;;; For most environments we have to copy, as the parent environments
;;; will be different.

(defmethod compile-time ((environment lexical-variable)
			 &key variable-shadow function-shadow)
  (compile-time (next environment)
    :variable-shadow (cons (name environment) variable-shadow)
    :function-shadow function-shadow))

(defmethod compile-time ((environment function)
			 &key variable-shadow function-shadow)
  (compile-time (next environment)
    :variable-shadow variable-shadow
    :function-shadow (cons (name environment) function-shadow)))

(defmethod compile-time ((environment special-variable)
			 &key variable-shadow function-shadow)
  (if (member (name environment) variable-shadow)
      (compile-time (next environment)
	:variable-shadow variable-shadow
	:function-shadow function-shadow)
      (make-instance 'special-variable
	:name (name environment)
	:next (compile-time (next environment)
		:variable-shadow (cons (name environment) variable-shadow)
		:function-shadow function-shadow))))

(defmethod compile-time ((environment symbol-macro)
			 &key variable-shadow function-shadow)
  (if (member (name environment) variable-shadow)
      (compile-time (next environment)
	:variable-shadow variable-shadow :function-shadow function-shadow)
      (make-instance 'symbol-macro
	:name (name environment)
	:expansion (expansion environment)
	:next (compile-time (next environment)
		:variable-shadow (cons (name environment) variable-shadow)
		:function-shadow function-shadow))))

(defmethod compile-time ((environment macro)
			 &key variable-shadow function-shadow)
  (if (member (name environment) function-shadow)
      (compile-time (next environment)
	:variable-shadow variable-shadow :function-shadow function-shadow)
      (make-instance 'macro
	:name (name environment)
	:expander (expander environment)
	:next (compile-time (next environment)
		:variable-shadow variable-shadow
		:function-shadow (cons (name environment) function-shadow)))))

(macrolet ((defcopy (class shadow &rest initargs-and-readers)
	     `(defmethod compile-time ((environment ,class)
				       &key variable-shadow function-shadow)
		(if (member (name environment) ,shadow)
		    (compile-time (next environment)
		      :variable-shadow variable-shadow
		      :function-shadow function-shadow)
		    (make-instance ',class
		      ,@(loop for (initarg reader) on initargs-and-readers
			        by #'cddr
			      collect initarg
			      collect `(,reader environment))
		      :next (compile-time (next environment)
			      :variable-shadow variable-shadow
			      :function-shadow function-shadow))))))
  (defcopy variable-type variable-shadow :name name :type type)
  (defcopy function-type function-shadow :name name :type type)
  (defcopy variable-ignore variable-shadow :name name :ignore ignore)
  (defcopy function-ignore function-shadow :name name :ignore ignore)
  (defcopy variable-dynamic-extent variable-shadow :name name)
  (defcopy function-dynamic-extent function-shadow :name name)
  (defcopy inline function-shadow :name name :inline inline)
  (defcopy inline-expansion function-shadow :name name :ast ast))

(defmethod compile-time ((environment optimize) &key variable-shadow function-shadow)
  (make-instance 'optimize
    :optimize (optimize environment) :policy (policy environment)
    :next (compile-time (next environment)
	    :variable-shadow variable-shadow
	    :function-shadow function-shadow)))
