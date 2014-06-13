(in-package #:sicl-compiler-phase-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting code to an abstract syntax tree.

;;; When this variable is false, we are invoked by EVAL or COMPILE.
;;; When it is true, we are invoked by COMPILE-FILE.
(defparameter *compile-file* nil)

;;; This variable holds the processing mode for top-level forms
;;; processed by COMPILE-FILE.
(defparameter *compile-time-too* nil)

;;; This variable is TRUE as long as a form is considered to be a
;;; top-level form.  It is bound to the value of *TOP-LEVEL-SUBFORM-P*
;;; before a subform is processed.
(defparameter *top-level-form-p* t)

;;; The value of this variable is normally FALSE.  It is bound to the
;;; value of *TOP-LEVEL-FORM-P* by certain converters that need to
;;; process subforms the same way as the form itself.
(defparameter *top-level-subform-p* t)

;;; When this variable is false, non-immediate constants will be
;;; converted into a LOAD-TIME-VALUE ast, which means that the machine
;;; code generated will be an access to an element in the vector of
;;; externals.  When it is true, such constants will instead be turned
;;; into code for creating them.
(defparameter *compile-for-linker* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a constant.
;;;

(defun convert-string (string)
  (let ((contents-var (gensym))
	(string-var (gensym)))
    (convert
     `(let ((,contents-var
	      (sicl-word:memalloc
	       (sicl-word:word ,(* 4 (1+ (length string)))))))
	(sicl-word:memset ,contents-var ,(length string))
	,@(loop for char across string
		for i from 4 by 4
		collect `(sicl-word:memset
			  (sicl-word:u+ ,contents-var
					(sicl-word:word ,i))
			  ,char))
	(let ((,string-var
		(sicl-word:memalloc (sicl-word:word 8))))
	  (sicl-word:memset
	   ,string-var
	   (sicl-word:memref
	    (sicl-word:word ,(+ (ash 1 30) 20))))
	  (sicl-word:memset
	   (sicl-word:u+ ,string-var (sicl-word:word 4))
	   ,contents-var)
	  (sicl-word:u+ ,string-var (sicl-word:word 3))))
     nil)))

;;; This function is only called for constants that can not
;;; be represented as immediates. 
(defun convert-constant-for-linker (constant)
  (cond ((stringp constant)
	 (convert-string constant))
	((symbolp constant)
	 (convert `(find-symbol
		    ,(symbol-name constant)
		    (find-package
		     ,(package-name (symbol-package constant))))
		  nil))))	 

(defgeneric convert (form environment))

(defun convert-initial (form)
  (let ((*location-asts* (make-hash-table :test #'eq)))
    (convert form nil)))

(defun convert-top-level-form (form)
  (let ((*location-asts* (make-hash-table :test #'eq)))
    (convert `(function (lambda () ,form)) nil)))

(defun convert-top-level-lamda-expression (lambda-expression)
  (unless (and (consp lambda-expression)
	       (eq (car lambda-expression) 'lambda))
    (error "argument must be a lambda expression"))
  (let ((*location-asts* (make-hash-table :test #'eq)))
    (convert `(function ,lambda-expression) nil)))

(defun convert-for-inlining (lambda-expression)
  (let* ((lambda-list (cadr lambda-expression))
	 (let-bindings (loop for var in lambda-list
			     for i from 0
			     collect `(,var (arg ,i))))
	 (*location-asts* (make-hash-table :test #'eq)))
    (let ((ast (convert `(let ,let-bindings ,@(cddr lambda-expression)) nil)))
      ;; The AST looks like this:
      ;; (progn (setq <a0> (arg 0)) (progn (setq <a1> (arg 1)) ....
      (loop for arg in lambda-list
	    collect (first (sicl-ast:children (first (sicl-ast:children ast))))
	      into lexical-asts
	    do (setf ast (second (sicl-ast:children ast)))
	    finally (return (values lexical-asts ast))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(define-condition situations-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition invalid-eval-when-situation
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'eval-when)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (unless (sicl-code-utilities:proper-list-p (cadr form))
    (error 'situations-must-be-proper-list
	   :expr (cadr form)))
  ;; Check each situation
  (loop for situation in (cadr form)
	do (unless (and (symbolp situation)
			(member situation
				'(:compile-toplevel :load-toplevel :execute
				  compile load eval)))
	     ;; FIXME: perhaps we should warn about the deprecated situations
	     (error 'invalid-eval-when-situation
		    :expr situation)))
  (let* ((situations (cadr form))
	 (c (not (null (intersection '(:compile-toplevel compile) situations))))
	 (l (not (null (intersection '(:load-toplevel load) situations))))
	 (e (not (null (intersection '(:execute eval) situations)))))
    (let ((new-form (cons 'progn (cddr form))))
      (if (and *compile-file* *top-level-form-p*)
	  (let ((*top-level-subform-p* *top-level-form-p*))
	    ;; This test tree corresponds to figure 3-7 of the
	    ;; HyperSpec.
	    (if c
		(if l
		    (let ((*compile-time-too* t))
		      (convert new-form environment))
		    (progn (eval new-form)
			   (convert ''nil environment)))
		(if l
		    (if e
			(convert new-form environment)
			(let ((*compile-time-too* nil))
			  (convert new-form environment)))
		    (if (and e *compile-time-too*)
			(progn (eval new-form)
			       (convert ''nil environment))
			(convert ''nil environment)))))
	  (if e
	      (convert new-form environment)
	      (convert ''nil environment))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting code for low-level operators.

(defun convert-arguments (arguments env)
  (loop for argument in arguments
	collect (convert argument env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting WORD.

(defmethod convert-compound ((symbol (eql 'sicl-word:word)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (sicl-ast:make-word-ast (cadr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MEMREF.

(defmethod convert-compound ((symbol (eql 'sicl-word:memref)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-memref-ast
   (car (convert-arguments (list (cadr form)) env))
   (caddr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MEMSET.

(defmethod convert-compound ((symbol (eql 'sicl-word:memset)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-memset-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting U+.

(defmethod convert-compound ((symbol (eql 'sicl-word:u+)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 3)
  (sicl-ast:make-u+-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting U-.

(defmethod convert-compound ((symbol (eql 'sicl-word:u-)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 3)
  (sicl-ast:make-u--ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting S+.

(defmethod convert-compound ((symbol (eql 'sicl-word:s+)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-s+-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting S-.

(defmethod convert-compound ((symbol (eql 'sicl-word:s-)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-s--ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting NEG.

(defmethod convert-compound ((symbol (eql 'sicl-word:neg)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (sicl-ast:make-neg-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting &.

(defmethod convert-compound ((symbol (eql 'sicl-word:&)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-&-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IOR.

(defmethod convert-compound ((symbol (eql 'sicl-word:ior)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-ior-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting XOR.

(defmethod convert-compound ((symbol (eql 'sicl-word:xor)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-xor-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting ~.

(defmethod convert-compound ((symbol (eql 'sicl-word:~)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-~-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting ==.

(defmethod convert-compound ((symbol (eql 'sicl-word:==)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-==-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting S<.

(defmethod convert-compound ((symbol (eql 'sicl-word:s<)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-s<-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting S<=.

(defmethod convert-compound ((symbol (eql 'sicl-word:s<=)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-s<=-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting U<.

(defmethod convert-compound ((symbol (eql 'sicl-word:u<)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-u<-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting U<=.

(defmethod convert-compound ((symbol (eql 'sicl-word:u<=)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-u<=-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT is the function that must be called by every conversion
;;; attempt.  
;;;
;;; This function has an unusual control structure.  The reason is the
;;; complication that arises because of the way top-level forms are
;;; handled.  Whether a form is a top-level form or not is indicated
;;; by the variable *TOP-LEVEL-FORM-P*.  This variable is initially
;;; TRUE, and we want it to become FALSE by default whenever CONVERT
;;; is called recursively.  In some cases, however, subforms of some
;;; form F should be processed the same way as F is processed (see
;;; section 3.2.3.1 in the HyperSpec).  The simplest control structure
;;; would be for all indivudual converters to set *TOP-LEVEL-FORM-P*
;;; to FALSE, EXCEPT those that process subforms the same way.
;;; However, that would require us to take special action whenever we
;;; add a converter for a new special form, and we would like to avoid
;;; that.  We would like the special action to be visible only in the
;;; converters that treat subforms as top-level forms.
;;;
;;; The way we solve this problem is as follows: We use a second
;;; variable *TOP-LEVEL-SUBFORM-P*, which is normally FALSE.  An
;;; :AROUND method on CONVERT binds *TOP-LEVEL-FORM-P* to the value of
;;; *TOP-LEVEL-SUBFORM-P*, and binds *TOP-LEVEL-SUBFORM-P* to FALSE.
;;; Individual converters that need to process subforms the same way
;;; bind *TOP-LEVEL-SUBFORM-P* to the value of *TOP-LEVEL-FORM-P*
;;; before calling CONVERT.


(defmethod convert (form environment)
  (setf form (sicl-env:fully-expand-form form environment))
  (cond ((and (not (consp form))
	      (not (symbolp form)))
	 (convert-constant form))
	((and (symbolp form)
	      (sicl-env:constantp form environment))
	 (convert-constant (sicl-env:symbol-value form)))
	((and (consp form)
	      (eq (car form) 'quote))
	 (convert-constant (cadr form)))
	((symbolp form)
	 (convert-variable form environment))
	(t
	 (convert-compound (car form) form environment))))
	 
(defmethod convert :around (form environment)
  (let ((*top-level-form-p* *top-level-subform-p*)
	(*top-level-subform-p* nil))
    (when (and *compile-time-too* *top-level-form-p*)
      (eval form))
    (call-next-method)))
