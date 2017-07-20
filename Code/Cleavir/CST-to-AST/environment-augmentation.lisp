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
