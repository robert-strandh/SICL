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
