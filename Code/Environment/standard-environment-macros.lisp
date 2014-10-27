(in-package #:sicl-standard-environment-macros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFCONSTANT.
;;;
;;; The HyperSpec says that we have a choice as to whether the
;;; initial-value form is evaluated at compile-time, at load-time, or
;;; both, but that in either case, the compiler must recognize the
;;; name as a constant variable.  We have chosen to evaluate it both
;;; at compile-time and at load-time.  We evaluate it at compile time
;;; so that successive references to the variable can be replaced by
;;; the value.
;;;
;;; This is not the final version of the macro.  For one thing, we
;;; need to handle the optional DOCUMENTATION argument.

(defmacro defconstant (name initial-value &optional documentation)
  (declare (ignore documentation))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (sicl-env:constant-variable ,name *global-environment*)
	   ,initial-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFVAR.
;;;
;;; The HyperSpec says that when DEFVAR is processed as a top-level
;;; form, then the rest of the compilation must treat the variable as
;;; special, but the initial-value form must not be evaluated, and
;;; there must be no assignment of any value to the variable.
;;;
;;; This is not the final version of DEFVAR, because we ignore the
;;; documentation for now.

(defmacro defvar
    (name &optional (initial-value nil initial-value-p) documentation)
  (declare (ignore documentation))
  (if initial-value-p
      `(progn
	 (eval-when (:compile-toplevel)
	   (setf (sicl-env:special-variable ,name *global-environment* nil)
		 nil))
	 (eval-when (:load-toplevel :execute)
	   (unless (sicl-env:boundp ,name *global-environment*)
	     (setf (sicl-env:special-variable ,name *global-environment* t)
		   ,initial-value))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (sicl-env:special-variable ,name *global-environment* nil)
	       nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFPARAMETER.
;;;
;;; The HyperSpec says that when DEFPARAMETER is processed as a
;;; top-level form, then the rest of the compilation must treat the
;;; variable as special, but the initial-value form must not be
;;; evaluated, and there must be no assignment of any value to the
;;; variable.
;;;
;;; This is not the final version of DEFPARAMETER, because we ignore
;;; the documentation for now.

(defmacro defparameter (name initial-value &optional documentation)
  (declare (ignore documentation))
  `(progn
     (eval-when (:compile-toplevel)
       (setf (sicl-env:special-variable ,name *global-environment* nil)
	     nil))
     (eval-when (:load-toplevel :execute)
       (setf (sicl-env:special-variable ,name *global-environment* t)
	     ,initial-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFTYPE.

(defmacro deftype (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (sicl-env:type-expander ,name *global-environment*)
	   (function ,(sicl-code-utilities:parse-deftype 
		       name
		       lambda-list
		       body))
	   ',name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFINE-COMPILER-MACRO.

(defmacro define-compiler-macro (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (sicl-env:compiler-macro-function ,name *global-environment*)
	   (function ,(sicl-code-utilities:parse-macro
		       name
		       lambda-list
		       body)))))
