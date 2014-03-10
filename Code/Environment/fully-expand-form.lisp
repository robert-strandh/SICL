(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FULLY-EXPAND-FORM.
;;;
;;; The compiler calls this function to "fully expand" a form.  While
;;; it might seem reasonable for the compiler to call MACROEXPAND,
;;; this is not always the case.  The reason is that MACROEXPAND
;;; repeatedly calls MACROEXPAND-1 until the second return value is
;;; false.  However, this is not the right thing for the compiler to
;;; do.  The reason is that it might well be the case that
;;; MACROEXPAND-1 returns a macro form that has a compiler macro
;;; associated with it.  If that is the case, then that compiler macro
;;; should be applied first.  It could also be the case that the
;;; MACROEXPAND-1 returns a form that is not a macro form, but a
;;; function call form with a compiler macro associated with it.  That
;;; compiler macro might then return a macro form.
;;;
;;; So instead of calling MACROEXPAND, the compiler calls this
;;; function.  It does the following:
;;;
;;;   * If the form is a symbol:
;;;
;;;      - If the most recent entry for symbol in the environment is a
;;;        symbol macro, then expand the form accordingly, and
;;;        iterate.
;;;
;;;      - If there is either no entry in the environment for the
;;;        symbol, or the most recent entry is not a not a symbol
;;;        macro, then we are done, and we return the original form.
;;;
;;;   * If the form is a compound form and the CAR of the compound
;;;     form is a symbol:
;;;
;;;      - If there is a local macro or a local function for the
;;;        symbol:
;;;
;;;         + If it is a local macro, then expand the form accordingly
;;;           and iterate.
;;;
;;;         + If it is a local function, we are done, and we return
;;;           the original form.
;;;
;;;      - If there is not a local macro or a local function for the
;;;        symbol, but there is a global macro for the symbol:
;;;
;;;         + If that global macro has a compiler macro associated
;;;           with it, then call the compiler-macro function to expand
;;;           the form, and iterate. 
;;;
;;;         + If that global macro does not have a compiler macro
;;;           associated with it, then expand the macro form and
;;;           iterate.
;;;
;;;      - If there is nether a local macro, a local function, nor a
;;;        global macro for the symbol, but there is a global function
;;;        for it:
;;;
;;;         + If that global function has a compiler macro associated
;;;           with it, then call the compiler-macro function to expand
;;;           the form, and iterate. 
;;;
;;;         + If that global function does not have a compiler macro
;;;           associated with it, then we are done, and we return the
;;;           original form.
;;;
;;;      - Otherwise (i.e., there is neither a local macro, a local
;;;        function, a global macro, nor a global function for the
;;;        symbol), we are done, and return the original form.
;;;
;;;   * Otherwise (i.e., the form is neither a symbol, nor a compound
;;;     form with a symbol in its CAR), we are done, and return the
;;;     original form.

(defun fully-expand-form (form environment)
  (loop
    do (cond ((symbolp form)
	      (let ((entry
		      (find-if (lambda (entry)
				 (and (typep entry 'variable-space)
				      (eq (name entry) form)))
			       (append environment
				       ;; There is no need to include
				       ;; any other lists to search,
				       ;; because we are really just
				       ;; interested in whether there
				       ;; is a symbol macro for the
				       ;; name.
				       (symbol-macros *global-environment*)))))
		(if (and (not (null entry))
			 (typep entry 'symbol-macro-entry))
		    (setf form
			  (funcall (coerce *macroexpand-hook* 'function)
				   (definition entry)
				   form
				   environment))
		    (return-from fully-expand-form form))))
	     ((and (consp form) (symbolp (car form)))
	      (let ((entry
		      (find-if (lambda (entry)
				 (and (typep entry 'function-space)
				      (eq (name entry) (car form))))
			       ;; We are only interested in local
			       ;; function or local macros, so no need
			       ;; to include the global environment.
			       environment)))
		(if (not (null entry))
		    ;; We found and entry corresponding either to a
		    ;; local macro or a local function. 
		    (if (typep entry 'local-macro-entry)
			;; It is a local macro.  Expand it and iterate. 
			(setf form
			      (funcall (coerce *macroexpand-hook* 'function)
				       (definition entry)
				       form
				       environment))
			;; It is a local function.  We are done. 
			(return-from fully-expand-form form))
		    ;; No local macro or local function found.  Check
		    ;; whether we have a global macro.
		    (let ((entry (find (car form) (macros *global-environment*)
				       :key #'name :test #'eq)))
		      (if (not (null entry))
			  ;; We found an entry corresponding to a
			  ;; global macro.  Check whether it has a
			  ;; compiler macro associated with it.
			  (let ((c-m-entry
				  (find entry
					(compiler-macros *global-environment*)
					:key #'base-entry :test #'eq)))
			    (if (not (null c-m-entry))
				;; We found a compiler macro entry.
				;; Try to expand it.
				(let ((new-form
					(funcall (coerce *macroexpand-hook*
							 'function)
						 (definition c-m-entry)
						 form
						 environment)))
				  (if (eq form new-form)
				      ;; The compiler macro declined.
				      ;; Expand the macro instead and
				      ;; iterate.
				      (setf form 
					    (funcall (coerce *macroexpand-hook*
							     'function)
						     (definition entry)
						     form
						     environment))
				      ;; The compiler macro expanded.
				      ;; Iterate with new form.
				      (setf form new-form)))
				;; No compiler macro found.  Expand the
				;; macro instead, and iterate
				(setf form 
				      (funcall (coerce *macroexpand-hook*
						       'function)
					       (definition entry)
					       form
					       environment))))
			  ;; No global macro found.  At this point, it
			  ;; is sufficient to search for a compiler
			  ;; macro, because it we find one, then it
			  ;; must be associated with a global
			  ;; function.
			  (let ((c-m-entry
				  (find-if (lambda (entry)
					     (eq (name (base-entry entry))
						 (car form)))
					   (compiler-macros
					    *global-environment*))))
			    (if (and (not (null c-m-entry))
				     (not (find (lambda (entry)
						  (and (typep entry
							      'notinline-declaration-entry)
						       (eq (base-entry entry)
							   (base-entry c-m-entry))))
						(proclamations *global-environment*))))
				;; We found a compiler macro entry.
				;; Try to expand it.
				(let ((new-form
					(funcall (coerce *macroexpand-hook*
							 'function)
						 (definition c-m-entry)
						 form
						 environment)))
				  (if (eq form new-form)
				      ;; The compiler macro declined.
				      ;; We are done.
				      (return-from fully-expand-form form)
				      ;; The compiler macro expanded.
				      ;; Iterate with new form
				      (setf form new-form)))
				;; No compiler macro found.  We are done.
				(return-from fully-expand-form form))))))))
	     (t (return-from fully-expand-form form)))))

