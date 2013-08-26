(in-package #:sicl-compiler-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFMACRO.
;;;
;;; We can obviously not define DEFMACRO using DEFMACRO.  But we have
;;; all the ingredients to define DEFMACRO "manually".

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'defmacro)
	(compile nil
		 (sicl-code-utilities:parse-macro
		  'defmacro
		  '(name lambda-list &body body)
		  '(`(eval-when (:compile-toplevel :load-toplevel :execute)
		       (setf (macro-function ',name)
			     ,(sicl-code-utilities:parse-macro
			       name
			       lambda-list
			       body))))))))

;;; The next form requires some explanation.  In the native compiler,
;;; the symbols defmacro and cl:defmacro are the same, so then this
;;; next form only redefines the macro defmacro.  In the cross
;;; compiler, however, the two symbols are different.  The effect of
;;; this form, then, is to define a host macro named
;;; sicl-compiler-environment:defmacro and which puts host functions
;;; into the global SICL environment as macro functions.  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro defmacro (name lambda-list &body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (macro-function ',name)
	     ,(sicl-code-utilities:parse-macro
	       name
	       lambda-list
	       body)))))

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
	   (ensure-defined-variable ,name))
	 (eval-when (:load-toplevel :execute)
	   (unless (boundp ,name)
	     (setf (symbol-value ,name) ,initial-value))))
      `(ensure-defined-variable ,name)))
		     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro LAMBDA.

(defmacro lambda (lambda-list &body body)
  `(function (lambda ,lambda-list ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFUN.
;;;
;;; For now, we don't use setf.
;;; 
;;; FIXME: handle documentation.
;;; FIXME: do more syntax checking.

(defmacro defun (name lambda-list &body body)
  (multiple-value-bind (declarations documentation forms)
      (sicl-code-utilities:separate-function-body body)
    `(funcall #'(setf fdefinition)
	      (lambda ,lambda-list
		,@(if (null documentation)
		      '()
		      `(,documentation))
		,@declarations
		(block ,name
		  ,@forms))
	      ,name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro IN-PACKAGE.

(defun in-package-function (string-designator)
  (declare ((or character symbol string) string-designator))
  (setq *package* (find-package string-designator)))

(defmacro in-package (string-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (in-package-function ,string-designator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A few macros that really belong elsewhere, but that 
;;; we include here now for testing purposes.

(defmacro when (test &body body)
  `(if ,test (progn ,@body) nil))

(defmacro unless (test &body body)
  `(if ,test nil (progn ,@body)))

(defmacro return (x)
  `(return-from nil ,x))

(defmacro and (&rest forms)
  (labels ((aux (forms)
	     (if (null (cdr forms))
		 (car forms)
		 `(if ,(car forms)
		      ,(aux (cdr forms))
		      nil))))
    (if (null forms)
	t
	(aux forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros that should not exist at all, but that we include in order
;;; to continue with the backend work.

(defmacro binary-< (x y)
  `(sicl-word:s< ,x ,y))

(defmacro binary-> (x y)
  `(sicl-word:s< ,y ,x))
