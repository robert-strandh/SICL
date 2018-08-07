(cl:in-package #:sicl-minimal-extrinsic-environment)

;;; This version of the function LOAD takes two environment objects.
;;; See section 3.2.1 in the HyperSpec for a description of the role
;;; of COMPILATION-ENVIRONMENT.  The LINKAGE-ENVIRONMENT is similar to
;;; what is called the RUN-TIME environment in section 3.2.1.  For
;;; SICL, it is more appropriate to call it the LINKAGE-ENVIRONMENT.
;;; It is the environment used to look up runtime definitions of
;;; functions and variables, whereas the code could very well be
;;; executed in a different environment.
(defun cst-load-source-with-environments
    (file compilation-environment linkage-environment system)
  (sicl-source-tracking:with-source-tracking-stream-from-file (stream file)
    (let ((*package* (sicl-genv:special-variable '*package*
                                                 compilation-environment)))
      (loop with eof = (list nil)
	    for cst = (eclector.concrete-syntax-tree:cst-read stream nil eof)
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

(defun cst-load-file (filename environment system)
  (format *trace-output* "CST Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (cst-load-source-with-environments
   (asdf:system-relative-pathname '#:sicl filename)
   environment
   environment
   system))
