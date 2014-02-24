(cl:in-package #:sicl-clos)

;;; In this version of COMPUTE-DISCRIMINATING-FUNCTION-DEFAULT, we DO
;;; use the compiler.  For a version that does not use the compiler,
;;; see the file compute-discriminating-function-support-b.lisp.

(defun make-discriminating-function-lambda (generic-function)
  (let* ((specializer-profile (specializer-profile generic-function))
	 ;; We do not use the Common Lisp function COUNT here, because
	 ;; we might want to define it as a generic function.
	 (active-arg-count (loop for x in specializer-profile
				 count x))
	 (class-number-vars (loop for x in specializer-profile
				  when x collect (gensym)))
	 (call-history (call-history generic-function)))
    ;; Check for the special case when the call history is empty.  In
    ;; that case, we just generate a call to the default
    ;; discriminating function.
    (when (null call-history)
      (return-from make-discriminating-function-lambda
	`(lambda (&rest arguments)
	   (default-discriminating-function ,generic-function
					    arguments
					    ',specializer-profile))))
    ;; Check for the special case when the active-arg-count is 0,
    ;; meaning that no method specialized on anything other than T,
    ;; but there is still some stuff in the call history, which is the
    ;; only possible effective method.  So we generate a call to that
    ;; effective method.
    (when (zerop active-arg-count)
      (return-from make-discriminating-function-lambda
	`(lambda (&rest arguments)
	   (apply ,(effective-method-cache (car call-history))
		  arguments))))
    ;; Create a dictionary, mapping effective methods to
    ;; forms containing APPLY that call those methods.
    (let ((dico '()))
      (loop for call-cache in call-history
	    for class-number-cache = (class-number-cache call-cache)
	    for effective-method = (effective-method-cache call-cache)
	    do (when (null (assoc effective-method dico :test #'eq))
		 (push (cons effective-method
			     `(return-from b
				(apply ,effective-method arguments)))
		       dico)))
      ;; Create a discriminating automaton with the entries in the call
      ;; history.
      (let ((automaton (make-automaton (1+ active-arg-count))))
	(loop for call-cache in call-history
	      for class-number-cache = (class-number-cache call-cache)
	      for effective-method = (effective-method-cache call-cache)
	      for action = (cdr (assoc effective-method dico :test #'eq))
	      do (add-path automaton class-number-cache action))
	(let* ((info (extract-transition-information automaton))
	       (tagbody (compute-discriminating-tagbody info class-number-vars)))
	  `(lambda (&rest arguments)
	     (block b
	       (let ,(loop for x in specializer-profile
			   for i from 0
			   when x collect `(,(nth i class-number-vars)
					    (instance-class-number
					     (nth ,i arguments))))
		 ,tagbody
		 (default-discriminating-function ,generic-function
						  arguments
						  ',specializer-profile)))))))))

;;; This function takes a generic function an returns a discriminating
;;; function for it that has the GENERIC-FUNCTION argument compiled in
;;; as a constant, so that the discriminating function can pass the
;;; generic function to the default discriminating function.
(defun make-default-discriminating-function (generic-function)
  (compile
   nil
   (make-discriminating-function-lambda generic-function)))

(defun compute-discriminating-function-default (generic-function)
  (make-default-discriminating-function generic-function))
