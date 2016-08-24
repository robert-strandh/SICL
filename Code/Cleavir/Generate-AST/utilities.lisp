(cl:in-package #:cleavir-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenience functions for augmenting the environment with a set of
;;; canonicalized declaration specifiers.
;;;
;;; Recall that a canonicalized declaration specifier is one of
;;; following:
;;;
;;;   * (declaration name)
;;;   * (dynamic-extent var)
;;;   * (dynamic-extent (function fn))
;;;   * (ftype type function-name)
;;;   * (ignore var)
;;;   * (ignore (function fn))
;;;   * (ignorable var)
;;;   * (ignorable (function fn))
;;;   * (inline function-name)
;;;   * (notinline function-name)
;;;   * (optimize (quality value))
;;;   * (special var)
;;;   * (type typespec var)

;;; Augment the environment with a single canonicalized declartion
;;; specifier.
(defun augment-environment-with-declaration
    (canonicalized-declaration-specifier environment)
  (destructuring-bind (head . rest) canonicalized-declaration-specifier
    (case head
      ;; (declaration
      ;; (make-declaration-declaration-entry (car rest)))
      (dynamic-extent
       (let ((var-or-function (car rest)))
	 (if (consp var-or-function)
	     (cleavir-env:add-function-dynamic-extent
	      environment (cadr var-or-function))
	     (cleavir-env:add-variable-dynamic-extent
	      environment (car var-or-function)))))
      (ftype
       (cleavir-env:add-function-type
	environment (cadr rest) (car rest)))
      ((ignore ignorable)
       (if (consp (car rest))
	   (cleavir-env:add-function-ignore
	    environment (cadr (car rest)) head)
	   (cleavir-env:add-variable-ignore
	    environment (car rest) head)))
      ((inline notinline)
       (cleavir-env:add-inline
	environment (car rest) head))
      (optimize
       (destructuring-bind (quality value) (car rest)
	 (cleavir-env:add-optimize environment quality value)))
      (special
       ;; This case is a bit tricky, because if the
       ;; variable is globally special, nothing should
       ;; be added to the environment.
       (let ((info (cleavir-env:variable-info environment (car rest))))
	 (if (and (typep info 'cleavir-env:special-variable-info)
		  (cleavir-env:global-p info))
	     environment
	     (cleavir-env:add-special-variable environment (car rest)))))
      (type
       (cleavir-env:add-function-type
	environment (cadr rest) (car rest)))
      (t
       (warn "Unable to handle declarations specifier: ~s"
	     canonicalized-declaration-specifier)
       environment))))

;;; Augment the environment with a list of canonicalized declartion
;;; specifiers.
(defun augment-environment-with-declarations (environment canonicalized-dspecs)
  (let ((new-env environment))
    (loop for spec in canonicalized-dspecs
	  do (setf new-env (augment-environment-with-declaration spec new-env)))
    new-env))

;;; Given a single variable bound by some binding form, a list of
;;; canonicalized declaration specifiers, and an environment in which
;;; the binding form is compiled, return true if and only if the
;;; variable to be bound is special.  Return a second value indicating
;;; whether the variable is globally special.
(defun variable-is-special-p (variable declarations env)
  (let ((existing-var-info (cleavir-env:variable-info env variable)))
    (cond ((typep existing-var-info 'cleavir-env:special-variable-info)
	   ;; It is mentioned in the environment as special.
	   (values t (cleavir-env:global-p existing-var-info)))
	  ((member `(special ,variable) declarations :test #'equal)
	   ;; If it is not mentioned in the environment, then it is
	   ;; special only if it is declared special.
	   (values t nil))
	  (t
	   (values nil nil)))))

;;; Given a list of canonicalized declaration specifiers for a single
;;; varible.  Return a type specifier resulting from all the type
;;; declarations present in the list.
(defun declared-type (declarations)
  `(and ,@(loop for declaration in declarations
		when (eq (car declaration) 'type)
		  collect (cadr declaration))))

;;; Given a single variable bound by some binding form like LET or
;;; LET*, and a list of canonicalized declaration specifiers
;;; concerning that variable, return a new environment that contains
;;; information about that variable.
;;;
;;; ENV is the environment to be augmented.  If the binding form has
;;; several bindings, it will contain entries for the variables
;;; preceding the one that is currently treated. 
;;;
;;; ORIG-ENV is the environment in which we check whether the variable
;;; is globally special.  For a LET form, this is the environment in
;;; which the entire LET form was converted.  For a LET* form, it is
;;; the same as ENV.
(defun augment-environment-with-variable
    (variable declarations env orig-env)
  (let ((new-env env))
    (multiple-value-bind (special-p globally-p)
	(variable-is-special-p variable declarations orig-env)
      (if special-p
	  (unless globally-p
	    (setf new-env
		  (cleavir-env:add-special-variable new-env variable)))
	  (let ((var-ast (cleavir-ast:make-lexical-ast variable)))
	    (setf new-env
		  (cleavir-env:add-lexical-variable new-env variable var-ast)))))
    (let ((type (declared-type declarations)))
      (unless (equal type '(and))
	(setf new-env
	      (cleavir-env:add-variable-type new-env variable type))))
    (when (member 'dynamic-extent declarations :test #'eq :key #'car)
      (setf new-env
	    (cleavir-env:add-variable-dynamic-extent new-env 'variable)))
    new-env))

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

;;; This function takes two arguments.  The first argument, VARIABLES,
;;; is a list of items, where each item is a non-empty list of symbols
;;; that are bound in a single binding.  The second items
;;; CANONICAL-DSPECS, is a list of canonicalized declaration
;;; specifiers.  This function returns a two values.  The first return
;;; value is a list with the same length as VARIABLES.  Each element
;;; in that list contains the elements in CANONICAL-DSPECS that apply
;;; to the corresponding element in VARIABLES.  The second return
;;; value is a list of the remaining declaration specifiers in
;;; CANONICAL-DSPECS i.e. the ones that do not apply to any element in
;;; VARIABLES.  A particular symbol S can not appear twice in an item
;;; of VARIABLES, but it can appear in different items.  In that case,
;;; the declaration specifiers that apply to that symbol will be
;;; associated with the last item in the list of VARIABLES.
(defun itemize-declaration-specifiers (variables canonical-dspecs)
  (if (null variables)
      (values '() canonical-dspecs)
      (multiple-value-bind (itemized-dspecs remaining-dspecs)
	  (itemize-declaration-specifiers (cdr variables) canonical-dspecs)
	(let ((item-specific-dspecs '()))
	  (loop for var in (first variables)
		do (multiple-value-bind (is-dspecs r-dspecs)
		       (separate-declarations remaining-dspecs var)
		     (setf item-specific-dspecs
			   (append is-dspecs item-specific-dspecs))
		     (setf remaining-dspecs r-dspecs)))
	  (values (cons item-specific-dspecs itemized-dspecs)
		  remaining-dspecs)))))

(defgeneric convert-special-binding
    (variable value-ast next-ast env system))

(defmethod convert-special-binding
    (variable value-ast next-ast global-env system)
  (let* ((function-name 'cleavir-primop:call-with-variable-bound)
	 (info (cleavir-env:function-info global-env function-name)))
    (assert (not (null info)))
    (cleavir-ast:make-call-ast
     (convert-global-function info global-env system)
     (list (cleavir-ast:make-load-time-value-ast `',variable t)
	   value-ast
	   (cleavir-ast:make-function-ast next-ast '())))))

;;; ENV is an environment that is known to contain information about
;;; the variable VARIABLE, but we don't know whether it is special or
;;; lexical.  VALUE-AST is an AST that computes the value to be given
;;; to VARIABLE.  NEXT-AST is an AST that represents the computation
;;; to take place after the variable has been given its value.  If the
;;; variable is special, this function creates a BIND-AST with
;;; NEXT-AST as its body.  If the variable is lexical, this function
;;; creates a PROGN-AST with two ASTs in it.  The first one is a
;;; SETQ-AST that assigns the value to the variable, and the second
;;; one is the NEXT-AST.
(defun set-or-bind-variable (variable value-ast next-ast env system)
  (let ((info (cleavir-env:variable-info env variable)))
    (assert (not (null info)))
    (if (typep info 'cleavir-env:special-variable-info)
	(let ((global-env (cleavir-env:global-environment env)))
	  (convert-special-binding
	   variable value-ast next-ast global-env system))
	(cleavir-ast:make-progn-ast
	 (list (cleavir-ast:make-setq-ast
		(cleavir-env:identity info)
		value-ast)
	       next-ast)))))

;;; Given a type in values position (i.e. argument to THE or return
;;;  value of a function type), return three values: a list of REQUIRED
;;;  types, a list of OPTIONAL types, a REST type, and whether REST
;;;  was present (to find &rest nil). e.g.
;;; (values integer &optional cons &rest symbol)
;;; => (INTEGER), (CONS), SYMBOL, T

;; FIXME: typexpand
(defun parse-values-type (values-type)
  (cond
    ((and (consp values-type) (eq (car values-type) 'values))
     (setf values-type (rest values-type))
     (values
      (loop while (and (consp values-type)
		       (not (find (car values-type)
				  '(&optional &rest))))
	    collect (pop values-type))
      (when (eq (car values-type) '&optional)
	(pop values-type)
	(loop while (and (consp values-type)
			 (not (eq (car values-type) '&rest)))
	      collect (pop values-type)))
      (when (eq (car values-type) '&rest)
	(assert (null (cddr values-type)))
	(second values-type))
      (eq (car values-type) '&rest)))
    (t (values (list values-type) nil nil))))

;;; Given two decomposed values types, return something like their
;;;  meet/conjunction.
;;; i.e., (the t1 form) and (the t2 form) iff (the result form),
;;;  except without THE's fudge.
;; FIXME: move this? it'll be useful later
;; assume these have been canonicalized, so rest does mean a type
(defun meet-values-types (req1 opt1 rest1 req2 opt2 rest2)
  (flet ((next-type-1 ()
	   (cond (req1 (pop req1)) (opt1 (pop opt1)) (t rest1)))
	 (next-type-2 ()
	   (cond (req2 (pop req2)) (opt2 (pop opt2)) (t rest2)))
	 (meet (t1 t2) `(and ,t1 ,t2)))
    (values
     (loop while (or req1 req2)
	   collect (meet (next-type-1) (next-type-2)))
     (loop while (or opt1 opt2)
	   collect (meet (next-type-1) (next-type-2)))
     (meet rest1 rest2))))
