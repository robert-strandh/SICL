(cl:in-package #:sicl-data-and-control-flow)

;;; FIXME: this definition is not finished yet.

(defmacro shiftf (&environment environment &rest arguments)
  (let* ((places (butlast arguments))
	 (new-value-form (first (last arguments)))
	 (setf-expansions
	   ;; Collect the SETF-EXPANSION of each place as a list of the
	   ;; values returned by GET-SETF-EXPANSION. 
	   (loop for place in places
		 collect (multiple-value-list
			  (get-setf-expansion place environment)))))
    (if (= (length places) 1)
	(multiple-value-bind (temporary-variables
			      value-forms
			      store-variables
			      storing-form
			      accessing-form)
	    (first setf-expansions)
	  `(let* ,(loop for var in temporary-variables
			for form in value-forms
			collect `(,var ,form))
	     (prog1 ,accessing-form
	       (multiple-value-bind ,store-variables ,new-value-form
		 ,storing-form))))
	(let ((result
		;; We start by creating the body of the result, which
		;; contains all the STORE-FORMs, storing the
		;; STORE-VARIABLEs in the respecctive place.
		`(progn ,@(loop for setf-expansion in setf-expansions
				for store-form = (fourth setf-expansion)
				collect store-form))))
	  (loop for right-hand-side
		  in (reverse (append (rest setf-expansions)
				      (list (first setf-expansions))))
		for store-variables = (third right-hand-side)
		for (temporary-variables
		     value-forms
		     nil
		     nil
		     accessing-form)
		  in (reverse setf-expansions)
		do (setf result
			 `(let* ,(loop for var in temporary-variables
				       for form in value-forms
				       collect `(,var ,form))
			    (multiple-value-bind ,store-variables ,accessing-form
			      ,result)))))
	result)))
