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
(defun augment-environment-with-declarations
    (environment declarations)
  (let ((canonicalized-declaration-specifiers
	  (cleavir-code-utilities:canonicalize-declaration-specifiers
	   (reduce #'append (mapcar #'cdr declarations))))
	(new-env environment))
    (loop for spec in canonicalized-declaration-specifiers
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
