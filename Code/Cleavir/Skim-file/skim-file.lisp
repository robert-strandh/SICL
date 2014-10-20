(cl:in-package #:cleavir-skim-file)

(defgeneric skim-special (symbol form environment))

(defun skim-form (form environment)
  (when (and (consp form) (symbolp (first form)))
    (let ((info (cleavir-env:function-info environment (first form))))
      (when (typep info 'cleavir-env:special-operator-info)
	(skim-special (first form) form environment)))))

(defun skim-file (filename environment)
  (with-open-file (stream filename :direction :input)
    (loop with eof-value = (list)
	  for top-level-form = (read stream nil eof-value)
	  until (eq top-level-form eof-value)
	  do (skim-form top-level-form environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenience functions for augmenting the environment with a bunch
;;; of declarations.
;;;
;;; FIXME: this code is identical to the code in the component
;;; Generate-AST/generate-ast.lisp.  Try to factor maybe.

(defun augment-environment-with-declaration (declaration environment)
  (destructuring-bind (head . rest) declaration
    (let ((new-env (case head
		     ;; (declaration
		     ;; (make-declaration-declaration-entry (car rest)))
		     (dynamic-extent
		      (if (consp (car rest))
			  (cleavir-env:add-function-dynamic-extent
			   environment (cadr (car rest)))
			  (cleavir-env:add-variable-dynamic-extent
			   environment (car rest))))
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
		     ;; (optimize
		     ;; (make-optimize-declaration-entry
		     ;; (car (car rest)) (cadr (car rest))))
		     ;; (special
		     ;; FIXME: is this right?
		     ;; (make-special-variable-entry (car rest)))
		     (type
		      (cleavir-env:add-function-type
		       environment (cadr rest) (car rest))))))
      new-env)))

(defun augment-environment-with-declarations (environment declarations)
  (let ((declaration-specifiers
	  (cleavir-code-utilities:canonicalize-declaration-specifiers
	   (reduce #'append (mapcar #'cdr declarations))))
	(new-env environment))
    (loop for spec in declaration-specifiers
	  do (setf new-env (augment-environment-with-declaration spec new-env)))
    new-env))
