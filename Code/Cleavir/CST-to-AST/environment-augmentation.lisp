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

