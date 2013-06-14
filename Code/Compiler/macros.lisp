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

(defun %defconstant (name initial-value)
  (unless (null (find name (special-variables *global-environment*)
		      :key #'name :test #'eq))
    (error "attempt to redefine a special variable as a constant variable."))
  (unless (null (find name (symbol-macros *global-environment*)
		      :key #'name :test #'eq))
    (error "attempt to redefine a global symbol macro as a constant variable."))
  (let ((existing-entry (find name (constant-variables *global-environment*)
			      :key #'name :test #'eq)))
    (cond ((null existing-entry)
	   (push (make-constant-variable-entry name initial-value)
		 (constant-variables *global-environment*)))
	  ((not (eql initial-value (definition existing-entry)))
	   (error "attempt to redefine a constant variable"))
	  (t
	   nil)))
  ;; Return the name as the HyperSpec requires
  name)

(defmacro defconstant (name initial-value &optional documentation)
  (declare (ignore documentation))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%defconstant ',name ,initial-value)))

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
       (ensure-defined-variable ,name))
     (eval-when (:load-toplevel :execute)
       (setf (symbol-value ,name) ,initial-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro LET*.
;;;
;;; The HyperSpec specifically allows for an implementation to
;;; implement any special operator as a macro.  We take advantage of
;;; that possibility by defining LET* to expand to nested LET forms,
;;; each with a single binding in it.
;;;
;;; The main complication here is that there might be declarations in
;;; the body of the LET*, and the ones that mention a variable being
;;; bound must go with the appropriate LET form.  We handle this
;;; situation by canonicalizing the declaration specifiers, and then
;;; dividing them up.

;;; Separate a list of canonicalized declaration specifiers into two
;;; disjoint sets, returned as two values.  The first set contains All
;;; the declerations specifiers that concern an ordinary variable
;;; named NAME, and the second set the remaining declaration specifiers.
(defun separate-declarations (canonicalized-declaration-specifiers name)
  (loop for spec in canonicalized-declaration-specifiers
	if (or (and (eq (first spec) 'ignore)
		    (eq (second spec) name))
	       (and (eq (first spec) 'ignorable)
		    (eq (second spec) name))
	       (and (eq (first spec) 'dynamic-extent)
		    (eq (second spec) name))
	       (and (eq (first spec) 'special)
		    (eq (second spec) name))
	       (and (eq (first spec) 'type)
		    (eq (third spec) name)))
	  collect spec into first
	else
	  collect spec into second
	finally (return (values first second))))

(defmacro let* (bindings &body body)
  ;; Check that the body is a proper list.
  (unless (sicl-code-utilities:proper-list-p body)
    (error "Body of LET* must be a proper list"))
  ;; Check that the bindings are in a proper list.
  (unless (sicl-code-utilities:proper-list-p bindings)
    (error "Bindings of LET* must be a proper list."))
  ;; Check the syntax of the bindings.
  (loop for binding in bindings
	do (unless (or (symbolp binding)
		       (and (sicl-code-utilities:proper-list-p binding)
			    (= (length binding) 2)
			    (symbolp (car binding))))
	     (error "Malformed LET binding: ~s" binding)))
  (if (= (length bindings) 1)
      `(let ,bindings ,@body)
      (multiple-value-bind (declarations forms)
	  (sicl-code-utilities:separate-ordinary-body body)
	(multiple-value-bind (first remaining)
	    (separate-declarations 
	     (sicl-code-utilities:canonicalize-declaration-specifiers 
	      (mapcar #'cdr declarations))
	     (if (symbolp (car bindings) (car bindings) (caar bindings))))
	  `(let (,(car bindings))
	     (declare ,@first)
	     (let* ,(cdr bindings)
	       (declare ,@remaining)
	       ,@forms))))))

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
