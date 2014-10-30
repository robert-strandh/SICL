(in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFMACRO.
;;;
;;; We can obviously not define DEFMACRO using DEFMACRO.  But we have
;;; all the ingredients to define DEFMACRO "manually".

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'defmacro)
	(compile nil
		 (cleavir-code-utilities:parse-macro
		  'defmacro
		  '(name lambda-list &body body)
		  '(`(eval-when (:compile-toplevel :load-toplevel :execute)
		       (setf (macro-function ',name)
			     ,(cleavir-code-utilities:parse-macro
			       name
			       lambda-list
			       body))))))))

;;; The next form requires some explanation.  In the native compiler,
;;; the symbols defmacro and cl:defmacro are the same, so then this
;;; next form only redefines the macro defmacro.  In the cross
;;; compiler, however, the two symbols are different.  The effect of
;;; this form, then, is to define a host macro named
;;; sicl-global-environment:defmacro and which puts host functions
;;; into the global SICL environment as macro functions.  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro defmacro (name lambda-list &body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (macro-function ',name)
	     ,(cleavir-code-utilities:parse-macro
	       name
	       lambda-list
	       body)))))

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
      (cleavir-code-utilities:separate-function-body body)
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
