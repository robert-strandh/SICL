(cl:in-package #:sicl-extrinsic-environment)

(defun load (file environment)
  (with-open-file (stream file :direction :input)
    (let ((*package* (sicl-genv:special-variable '*package* environment)))
      (loop with eof = (list nil)
	    for form = (sicl-reader:read stream nil eof)
	    until (eq form eof)
	    do (cleavir-env:eval form environment environment)
	       ;; The evaluation of the form might have changed the
	       ;; value of the variable *PACKAGE* in the target
	       ;; environment.  But this function is executed as a
	       ;; host function, so the next time we call READ, we
	       ;; need to make sure the host variable *PACKAGE* also
	       ;; changes.
	       (setf *package*
		     (sicl-genv:special-variable '*package* environment))))))

;;; This version of the function LOAD takes two environment objects.
;;; See section 3.2.1 in the HyperSpec for a description of the role
;;; of COMPILATION-ENVIRONMENT.  The LINKAGE-ENVIRONMENT is similar to
;;; what is called the RUN-TIME environment in section 3.2.1.  For
;;; SICL, it is more appropriate to call it the LINKAGE-ENVIRONMENT.
;;; It is the environment used to look up runtime definitions of
;;; functions and variables, whereas the code could very well be
;;; executed in a different environment.
(defun load-source-with-environments
    (file compilation-environment linkage-environment)
  (with-open-file (stream file :direction :input)
    (let ((*package* (sicl-genv:special-variable '*package*
                                                 compilation-environment)))
      (loop with eof = (list nil)
	    for form = (sicl-reader:read stream nil eof)
	    until (eq form eof)
	    do (cleavir-env:eval
		form compilation-environment linkage-environment)
	       ;; The evaluation of the form might have changed the
	       ;; value of the variable *PACKAGE* in the target
	       ;; environment.  But this function is executed as a
	       ;; host function, so the next time we call READ, we
	       ;; need to make sure the host variable *PACKAGE* also
	       ;; changes.
	       (setf *package*
		     (sicl-genv:special-variable '*package*
                                                 linkage-environment))))))

;;; This function is like LOAD-SOURCE-WITH-ENVIRONMENTS, except that
;;; it converts the form to a concrete-syntax-tree and then calls
;;; CLEAVIR-ENV:CST-EVAL instead of CLEAVIR-ENV:EVAL.
(defun cst-load-source-with-environments
    (file compilation-environment linkage-environment system)
  (with-open-file (stream file :direction :input)
    (let ((*package* (sicl-genv:special-variable '*package*
                                                 compilation-environment)))
      (loop with eof = (list nil)
	    for cst = (sicl-reader:cst-read stream nil eof)
	    until (eq cst eof)
	    do (cleavir-env:cst-eval
		cst compilation-environment linkage-environment system)
	       ;; The evaluation of the form might have changed the
	       ;; value of the variable *PACKAGE* in the target
	       ;; environment.  But this function is executed as a
	       ;; host function, so the next time we call READ, we
	       ;; need to make sure the host variable *PACKAGE* also
	       ;; changes.
	       (setf *package*
		     (sicl-genv:special-variable '*package*
                                                 linkage-environment))))))
