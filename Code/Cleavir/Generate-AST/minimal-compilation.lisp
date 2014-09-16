(in-package #:cleavir-generate-ast)

(defgeneric minimally-compile-special-form (symbol form env))

(defmethod minimally-compile-special-form :around (symbol form env)
  (declare (ignore env))
  (check-special-form-syntax symbol form))

(defun minimally-compile-sequence (sequence env)
  (loop for form in sequence
	collect (minimally-compile form env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling QUOTE.

(defmethod minimally-compile-special-form
    ((symbol (eql 'quote)) form env)
  (declare (ignore env))
  form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling BLOCK.

(defmethod minimally-compile-special-form
    ((symbol (eql 'block)) form env)
  `(block ,(second form)
     ,@(minimally-compile-sequence (rest (rest form)) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling EVAL-WHEN.

(defmethod minimally-compile-special-form
    ((symbol (eql 'eval-when)) form env)
  `(eval-when ,(second form)
     ,@(minimally-compile-sequence (rest (rest form)) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling FLET and LABELS.

(defun minimally-compile-optional-or-key (optional-or-key env)
  `(,(first optional-or-key)
    ,(minimally-compile (second optional-or-key) env)
    ,@(if (null (rest (rest optional-or-key)))
	  '()
	  (rest (rest optional-or-key)))))

(defun minimally-compile-lambda-list (lambda-list env)
  (let ((parsed-lambda-list
	  (cleavir-code-utilities:parse-ordinary-lambda-list lambda-list)))
    `(,@(cleavir-code-utilities:required parsed-lambda-list)
      ,@(let ((optionals (cleavir-code-utilities:optionals parsed-lambda-list)))
	  (eq optionals :none)
	  '()
	  `(&optional
	    ,@(loop for optional in optionals
		    collect (minimally-compile-optional-or-key optional env))))
      ,@(let ((keys (cleavir-code-utilities:keys parsed-lambda-list)))
	  (eq keys :none)
	  '()
	  `(&key
	    ,@(loop for key in keys
		    collect (minimally-compile-optional-or-key key env))))
      ,@(if (cleavir-code-utilities:allow-other-keys parsed-lambda-list)
	    '(&allow-other-keys)
	    '()))))

(defun minimally-compile-code (lambda-list body env)
  (multiple-value-bind (declarations documentation forms)
      (cleavir-code-utilities:separate-function-body body)
    `(,(minimally-compile-lambda-list lambda-list env)
      ,@declarations
      ,(if (null documentation)
	   '()
	   `(,documentation))
      ,@(minimally-compile-sequence forms env))))

(defun minimally-compile-flet-or-labels (symbol form env)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body (cddr form))
    `(,symbol ,(loop for (name lambda-list . local-body) in (second form)
		     collect `(name ,(minimally-compile-code
				      lambda-list local-body env)
				    ,@declarations
				    ,@(minimally-compile-sequence forms env))))))

(defmethod minimally-compile-special-form
    ((symbol (eql 'flet)) form env)
  (minimally-compile-flet-or-labels symbol form env))

(defmethod minimally-compile-special-form
    ((symbol (eql 'labels)) form env)
  (minimally-compile-flet-or-labels symbol form env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling FUNCTION.

(defun minimally-compile-lambda-function (lambda-form env)
  (destructuring-bind (lambda-list . body) lambda-form
    `(lambda
	 ,@(minimally-compile-code lambda-list body env))))

(defmethod minimally-compile-special-form
    ((symbol (eql 'function)) form env)
  `(function
    ,(if (proper-function-name-p (second form))
	 (second form)
	 (minimally-compile-lambda-function (second form) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling GO.

(defmethod minimally-compile-special-form
    ((symbol (eql 'go)) form env)
  (declare (ignore env))
  form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling IF.

(defmethod minimally-compile-special-form
    ((symbol (eql 'if)) form env)
  `(if ,(minimally-compile (second form) env)
       ,(minimally-compile (third form) env)
       ,(if (null (nthcdr 3 form))
	    nil
	    (minimally-compile (fourth form) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling LET and LET*

(defun minimally-compile-binding (binding env)
  (if (symbolp binding)
      `(,binding nil)
      `(,(first binding)
	,(if (null (rest binding))
	     nil
	     (minimally-compile (second binding) env)))))

(defmethod minimally-compile-special-form
    ((symbol (eql 'let)) form env)
  (destructuring-bind (bindings &rest body) (cdr form)
    (multiple-value-bind (declarations forms)
	(cleavir-code-utilities:separate-ordinary-body body)
      `(let ,(loop for binding in bindings
		   do (minimally-compile-binding binding env))
	 ,declarations
	 ,@(minimally-compile-sequence (rest (rest forms)) env)))))

(defmethod minimally-compile-special-form
    ((symbol (eql 'let*)) form env)
  (destructuring-bind (bindings &rest body) (cdr form)
    (multiple-value-bind (declarations forms)
	(cleavir-code-utilities:separate-ordinary-body body)
      `(let* ,(loop for binding in bindings
		    do (minimally-compile-binding binding env))
	 ,declarations
	 ,@(minimally-compile-sequence (rest (rest forms)) env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling LOAD-TIME-VALUE.

(defmethod minimally-compile-special-form
    ((symbol (eql 'load-time-value)) form environment)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 2)
  (cleavir-ast:make-load-time-value-ast
   (minimally-compile (cadr form) environment)
   (caddr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling LOCALLY.

(defmethod minimally-compile-special-form
    ((symbol (eql 'locally)) form env)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body (cdr form))
    (let ((new-env (augment-environment-with-declarations
		    env declarations)))
      (cleavir-ast:make-progn-ast
       (minimally-compile-sequence forms new-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling MACROLET.

;; According to section 3.2.3.1 of the HyperSpec, MACROLET processes
;; its subforms the same way as the form itself.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling PROGN.

(defmethod minimally-compile-special-form
    ((head (eql 'progn)) form environment)
  (cleavir-ast:make-progn-ast
   (minimally-compile-sequence (cdr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling RETURN-FROM.

(defmethod minimally-compile-special-form
    ((symbol (eql 'return-from)) form env)
  (let ((info (cleavir-env:block-info (cadr form) env)))
    (if (null info)
	(error 'block-name-unknown
	       :expr (cadr form))
	(cleavir-ast:make-return-from-ast
	 (cleavir-env:identity info)
	 (minimally-compile (caddr form) env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling SETQ.

(defmethod minimally-compile-special-form
    ((symbol (eql 'setq)) form env)
  `(setq
    ,@(loop for (var form) on (cdr form) by #'cddr
	    collect `(var (minimally-compile form env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling SYMBOL-MACROLET.

(defmethod minimally-compile-special-form
    ((head (eql 'symbol-macrolet)) form env)
  (let ((new-env env))
    (loop for (name expansion) in (second form)
	  do (setf new-env
		   (cleavir-env:add-local-symbol-macro new-env name expansion)))
    (minimally-compile `(progn ,@(rest (rest form))) new-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling TAGBODY.

(defmethod minimally-compile-special-form
    ((symbol (eql 'tagbody)) form env)
  `(tagbody
      ,@(loop for item in (rest form)
	      collect (if (symbolp item)
			  item
			  (minimally-compile item env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling THE.

(defmethod minimally-compile-special-form
    ((symbol (eql 'the)) form env)
  `(the ,(second form)
	,(minimally-compile (third form) env)))
