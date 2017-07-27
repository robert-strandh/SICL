(cl:in-package #:cleavir-cst-to-ast)

;;; Take an environment and the CST of a single function definition,
;;; and return a new environment which is like the one passed as an
;;; argument except that it has been augmented by the local function
;;; name.
(defun augment-environment-from-fdef (environment definition-cst)
  (cst:db origin (name . rest) definition-cst
    (declare (ignore rest))
    (let* ((raw-name (cst:raw name))
           (var-ast (cleavir-ast:make-lexical-ast raw-name
                                                  :origin origin)))
      (cleavir-env:add-local-function environment raw-name var-ast))))

;;; Take an environment, a CST representing list of function
;;; definitions, and return a new environment which is like the one
;;; passed as an argument, except that is has been augmented by the
;;; local function names in the list.
(defun augment-environment-from-fdefs (environment definitions-cst)
  (loop with result = environment
	for definition-cst = definitions-cst then (cst:rest definition-cst)
        until (cst:null definition-cst)
	do (setf result
		 (augment-environment-from-fdef result definition-cst))
	finally (return result)))

;;; Augment the environment with a single canonicalized declaration
;;; specifier.
(defgeneric augment-environment-with-declaration
    (declaration-identifier
     declaration-identifier-cst
     declaration-data-cst
     environment))

(defmethod augment-environment-with-declaration
    (declaration-identifier
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (warn "Unable to handle declarations specifier: ~s"
        declaration-identifier)
  environment))))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'dynamic-extent))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (let ((var-or-function (cst:first declaration-data-cst)))
    (if (cst:consp var-or-function)
        ;; (dynamic-extent (function foo))
        (cleavir-env:add-function-dynamic-extent
         environment (cst:second var-or-function))
        ;; (dynamic-extent foo)
        (cleavir-env:add-variable-dynamic-extent
         environment var-or-function))))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'ftype))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (cleavir-env:add-function-type
   environment (second declaration-data-cst) (first declaration-data-cst)))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'ignore))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (if (cst:consp (first declaration-data-cst))
      (cleavir-env:add-function-ignore
       environment
       (cst:second (cst:first declaration-data-cst))
       declaration-identifier-cst)
      (cleavir-env:add-variable-ignore
       environment
       (cst:first declaration-data-cst)
       declaration-identifier-cst)))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'ignorable))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (if (cst:consp (first declaration-data-cst))
      (cleavir-env:add-function-ignore
       environment
       (cst:second (cst:first declaration-data-cst))
       declaration-identifier-cst)
      (cleavir-env:add-variable-ignore
       environment
       (cst:first declaration-data-cst)
       declaration-identifier-cst)))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'inline))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (cleavir-env:add-inline
   environment (cst:first declaration-data-cst) declaration-identifier-cst))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'notinline))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (cleavir-env:add-inline
   environment (cst:first declaration-data-cst) declaration-identifier-cst))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'special))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  ;; This case is a bit tricky, because if the
  ;; variable is globally special, nothing should
  ;; be added to the environment.
  (let ((info (cleavir-env:variable-info environment
                                         (cst:first declaration-data-cst))))
    (if (and (typep info 'cleavir-env:special-variable-info)
             (cleavir-env:global-p info))
        environment
        (cleavir-env:add-special-variable environment
                                          (cst:first declaration-data-cst)))))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'type))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (cst:db source (type-cst variable-cst) declaration-data-cst
    (cleavir-env:add-variable-type environment variable-cst type-cst)))

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
    (when (member 'ignore declarations :test #'eq :key #'car)
      (setf new-env
            (cleavir-env:add-variable-ignore new-env variable 'ignore)))
    (when (member 'ignorable declarations :test #'eq :key #'car)
      (setf new-env
            (cleavir-env:add-variable-ignore new-env variable 'ignorable)))
    (when (member 'dynamic-extent declarations :test #'eq :key #'car)
      (setf new-env
	    (cleavir-env:add-variable-dynamic-extent new-env variable)))
    new-env))

;;; The only purpose of this function is to call the function
;;; AUGMENT-ENVIRONMENT-WITH-VARIABLE twice, once for the parameter
;;; variable and once for its associated supplied-p parameter, except
;;; that it also tests whether the supplied-p parameter is NIL,
;;; indicating that no supplied-p parameter was given.  This function
;;; returns the augmented environment.
(defun augment-environment-with-parameter (var supplied-p dspecs env)
  (let ((new-env (augment-environment-with-variable
		  var dspecs env env)))
    (if (null supplied-p)
	new-env
	(augment-environment-with-variable supplied-p dspecs new-env new-env))))
