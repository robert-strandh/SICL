(cl:in-package #:cleavir-generate-ast)

(defgeneric check-special-form-syntax (head form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking QUOTE.

(defmethod check-special-form-syntax ((head (eql 'quote)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking BLOCK.

(defmethod check-special-form-syntax ((head (eql 'block)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (unless (symbolp (cadr form))
    (error 'block-name-must-be-a-symbol
	   :expr (cadr form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking EVAL-WHEN.

(defmethod check-special-form-syntax ((head (eql 'eval-when)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (unless (cleavir-code-utilities:proper-list-p (second form))
    (error 'situations-must-be-proper-list
	   :expr (second form)))
  ;; Check each situation
  (loop for situation in (second form)
	do (unless (and (symbolp situation)
			(member situation
				'(:compile-toplevel :load-toplevel :execute
				  compile load eval)))
	     ;; FIXME: perhaps we should warn about the deprecated situations
	     (error 'invalid-eval-when-situation
		    :expr situation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FLET.

(defmethod check-special-form-syntax ((head (eql 'flet)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (unless (cleavir-code-utilities:proper-list-p (cadr form))
    (error 'flet-functions-must-be-proper-list
	   :expr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FUNCTION.

(defun proper-function-name-p (name)
  (or (symbolp name)
      (and (cleavir-code-utilities:proper-list-p name)
	   (= (length name) 2)
	   (eq (car name) 'setf)
	   (symbolp (cadr name)))))

(defmethod check-special-form-syntax ((head (eql 'function)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1)
  (cond ((proper-function-name-p (second form))
	 nil)
	((consp (second form))
	 (unless (eq (first (second form)) 'lambda)
	   (error 'function-argument-must-be-function-name-or-lambda-expression
		  :expr (second form)))
	 (unless (cleavir-code-utilities:proper-list-p (second form))
	   (error 'lambda-must-be-proper-list
		  :expr (second form))))
	(t
	 (error 'function-argument-must-be-function-name-or-lambda-expression
		:expr (cadr form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking GO.

(defmethod check-special-form-syntax ((head (eql 'go)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking IF.

(defmethod check-special-form-syntax ((head (eql 'if)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LABELS.

(defmethod check-special-form-syntax ((head (eql 'labels)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (unless (cleavir-code-utilities:proper-list-p (cadr form))
    (error 'labels-functions-must-be-proper-list
	   :expr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LET and LET*

(defun check-binding-forms (binding-forms)
  (unless (cleavir-code-utilities:proper-list-p binding-forms)
    (error 'bindings-must-be-proper-list :expr binding-forms))
  (loop for binding-form in binding-forms
	do (unless (or (symbolp binding-form) (consp binding-form))
	     (error 'binding-must-be-symbol-or-list
		    :expr binding-form))
	   (when (and (consp binding-form)
		      (or (not (listp (cdr binding-form)))
			  (not (null (cddr binding-form)))))
	     (error 'binding-must-have-length-one-or-two
		    :expr binding-form))
	   (when (and (consp binding-form)
		      (not (symbolp (car binding-form))))
	     (error 'variable-must-be-a-symbol
		    :expr (car binding-form)))))

(defmethod check-special-form-syntax
    ((head (eql 'let)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (check-binding-forms (second form)))

(defmethod check-special-form-syntax ((head (eql 'let*)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (check-binding-forms (second form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LOAD-TIME-VALUE.

(defmethod check-special-form-syntax ((head (eql 'load-time-value)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 2)
  (unless (null (cddr form))
    ;; The HyperSpec specifically requires a "boolean"
    ;; and not a "generalized boolean".
    (unless (member (caddr form) '(nil t))
      (error 'read-only-p-must-be-boolean
	     :expr (caddr form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LOCALLY.

(defmethod check-special-form-syntax ((head (eql 'locally)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:separate-ordinary-body (rest form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking MACROLET.

(defmethod check-special-form-syntax ((head (eql 'macrolet)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (unless (cleavir-code-utilities:proper-list-p (cadr form))
    (error 'macrolet-definitions-must-be-proper-list
	   :expr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking MULTIPLE-VALUE-CALL.

(defmethod check-special-form-syntax ((head (eql 'multiple-value-call)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking MULTIPLE-VALUE-PROG1.

(defmethod check-special-form-syntax ((head (eql 'multiple-value-prog1)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking PROGN.

(defmethod check-special-form-syntax ((head (eql 'progn)) form)
  (cleavir-code-utilities:check-form-proper-list form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking PROGV.

(defmethod check-special-form-syntax ((head (eql 'progv)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking RETURN-FROM.

(defmethod check-special-form-syntax ((head (eql 'return-from)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 2)
  (unless (symbolp (cadr form))
    (error 'block-name-must-be-a-symbol
	   :expr (cadr form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking SETQ.

(defmethod check-special-form-syntax ((head (eql 'setq)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (unless (oddp (length form))
    (error 'setq-must-have-even-number-of-arguments
	   :expr form))
  (loop for variable in (cdr form) by #'cddr
	unless (symbolp variable)
	  do (error 'setq-var-must-be-symbol
		    :expr variable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking SYMBOL-MACROLET.
;;;
;;; FIXME: syntax check bindings

(defmethod check-special-form-syntax ((head (eql 'symbol-macrolet)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking TAGBODY.

(defmethod check-special-form-syntax ((head (eql 'tagbody)) form)
  (cleavir-code-utilities:check-form-proper-list form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking THE.

(defmethod check-special-form-syntax ((head (eql 'the)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking THROW

(defmethod check-special-form-syntax ((head (eql 'throw)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking UNWIND-PROTECT

(defmethod check-special-form-syntax ((head (eql 'unwind-protect)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking CATCH.

(defmethod check-special-form-syntax ((head (eql 'catch)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax checks for the PRIMOPs.

;;; This macro can be used to define a simple syntax-check method,
;;; where the form must be a proper list and it has a fixed number of
;;; arguments.
(defmacro define-simple-check (operation argcount)
  `(defmethod check-special-form-syntax ((head (eql ',operation)) form)
     (cleavir-code-utilities:check-form-proper-list form)
     (cleavir-code-utilities:check-argcount form ,argcount ,argcount)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking EQ.

(defmethod check-special-form-syntax ((head (eql 'cleavir-primop:eq)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking CAR.

(defmethod check-special-form-syntax ((head (eql 'cleavir-primop:car)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking CDR.

(defmethod check-special-form-syntax ((head (eql 'cleavir-primop:cdr)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking RPLACA.

(defmethod check-special-form-syntax ((head (eql 'cleavir-primop:rplaca)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking RPLACD.

(defmethod check-special-form-syntax ((head (eql 'cleavir-primop:rplacd)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LET-UNINITIALIZED.

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:let-uninitialized)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (assert (cleavir-code-utilities:proper-list-p (second form)))
  (assert (every #'symbolp (second form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FUNCALL.

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:funcall)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking TYPEQ.

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:typeq)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-+

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum-+)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 3 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM--

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum--)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 3 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-<

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum-<)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-<=

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum-<=)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM->

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum->)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM->=

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum->=)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-=

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum-=)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-ADD

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum-add)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 3 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-SUB

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum-sub)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 3 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-LESS

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum-less)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-NOT-GREATER

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum-not-greater)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-GREATER

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum-greater)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-NOT-LESS

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum-not-less)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FIXNUM-equal

(defmethod check-special-form-syntax
    ((head (eql 'cleavir-primop:fixnum-equal)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))
