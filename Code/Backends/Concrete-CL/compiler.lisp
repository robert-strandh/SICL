(cl:in-package #:sicl-cross-compiler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main compiler functions.

(defun compile-form (form environment)
  (if (and (atom form) (not (symbolp form)))
      `((push-constant ',form)
	(pop-to-arg-immediate 0))
      (multiple-value-bind (new-form expanded-p)
	  (macroexpand form environment)
	(if expanded-p
	    (compile-form new-form environment)
	    (if (symbolp form)
		(let ((lexical-level
			(find-binding form
				      environment
				      'lexical-variable-environment)))
		  (if (null lexical-level)
		      `((comment ,(format nil "special variable ~s" form))
			(push-constant ',form)
			;; FIXME: We don't do special bindings yet.
			(push-symbol-value)
			(pop-to-arg-immediate 0)
			(push-immediate 1)
			(pop-to-arg-count))
		      `((comment ,(format nil "lexical variable ~s" form))
			(push-from-lexical ,lexical-level 0)
			(pop-to-arg-immediate 0)
			(push-immediate 1)
			(pop-to-arg-count))))
		(let ((special-compiler (special-operator (car form) environment)))
		  (if (not (null special-compiler))
		      (funcall special-compiler (cdr form) environment)
		      `((comment ,(format nil
					  "compiling call to function ~s"
					  (car form)))
			(comment ,(format nil "name ~s" (car form)))
			(push-constant ',(car form))
			,@(loop for subform in (cdr form)
				for arg from 0
				append
				`((comment ,(format nil "arg ~s of ~s" arg
						    (car form)))
				  ,@(compile-form subform environment)
				  (push-from-arg-immediate 0)))
			,@(reverse (loop for arg from 0 below (length (cdr form))
					 collect `(pop-to-arg-immediate ,arg)))
			(push-immediate ,(length (cdr form)))
			(pop-to-arg-count)
			(comment ,(format nil "calling function ~s"
					  (car form)))
			(call)))))))))

(defun cross-compile-file (filename)
  (let ((*package* (find-package '#:sicl-cl-implementation)))
    (with-open-file (stream filename :direction :input)
      (loop with eof = (list nil)
	    for form = (read stream nil eof)
	    until (eq form eof)
	    append (compile-form form *compiler-environment*)))))

(defun cross-compile-form (form)
  (compile-form form *compiler-environment*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some special operators.

;;; FIXME: define a function PROPER-LIST-P
;;; to check the code.  Check for circularity too. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Our low-level special operators.

;;; When we just give a constant number in code, it is
;;; (correctly) intepreted as a fixnum.  To supress that, we
;;; can say (sicl-system:word <number>) and it is interpreted
;;; as an immediate operand.
;;; FIXME: use (the sicl-system:word <number>) instead.
;;; FIXME: use (the t <expression>) to return to a Lisp expression.
;;;        or any valid Lisp type other than sicl-system:word.
(setf (special-operator 'sicl-system:word)
      (lambda (arguments environment)
	(declare (ignore environment))
	(assert (= (length arguments) 1))
	`((push-immediate ,(car arguments))
	  (pop-to-arg-immediate 0))))

(setf (special-operator 'sicl-exp-heap:malloc)
      (lambda (arguments environment)
	(assert (= (length arguments) 1))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  (malloc)
	  (pop-to-arg-immediate 0))))

(setf (special-operator 'sicl-system:memref)
      (lambda (arguments environment)
	(assert (= (length arguments) 1))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  (memref)
	  (pop-to-arg-immediate 0))))

(setf (special-operator 'sicl-system:memset)
      (lambda (arguments environment)
	(assert (= (length arguments) 2))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  ,@(compile-form (cadr arguments) environment)
	  (push-from-arg-immediate 0)
	  (memset))))

(setf (special-operator 'sicl-system:wu+)
      (lambda (arguments environment)
	(assert (= (length arguments) 2))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  ,@(compile-form (cadr arguments) environment)
	  (push-from-arg-immediate 0)
	  (unsigned-add)
	  (pop-to-arg-immediate 0))))

(setf (special-operator 'sicl-system:ws+)
      (lambda (arguments environment)
	(assert (= (length arguments) 2))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  ,@(compile-form (cadr arguments) environment)
	  (push-from-arg-immediate 0)
	  (signed-add)
	  (pop-to-arg-immediate 0))))

(setf (special-operator 'sicl-system:wu-)
      (lambda (arguments environment)
	(assert (= (length arguments) 2))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  ,@(compile-form (cadr arguments) environment)
	  (push-from-arg-immediate 0)
	  (unsigned-sub)
	  (pop-to-arg-immediate 0))))

(setf (special-operator 'sicl-system:ws-)
      (lambda (arguments environment)
	(assert (= (length arguments) 2))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  ,@(compile-form (cadr arguments) environment)
	  (push-from-arg-immediate 0)
	  (signed-sub)
	  (pop-to-arg-immediate 0))))

(setf (special-operator 'sicl-system:wneg)
      (lambda (arguments environment)
	(assert (= (length arguments) 1))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  (neg)
	  (pop-to-arg-immediate 0))))

(setf (special-operator 'sicl-system:wu*)
      (lambda (arguments environment)
	(assert (= (length arguments) 2))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  ,@(compile-form (cadr arguments) environment)
	  (push-from-arg-immediate 0)
	  (unsigned-mul)
	  ;; FIXME: is this the right order?
	  (pop-to-arg-immediate 0)
	  (pop-to-arg-immediate 1))))

(setf (special-operator 'sicl-system:ws*)
      (lambda (arguments environment)
	(assert (= (length arguments) 2))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  ,@(compile-form (cadr arguments) environment)
	  (push-from-arg-immediate 0)
	  (signed-mul)
	  ;; FIXME: is this the right order?
	  (pop-to-arg-immediate 0)
	  (pop-to-arg-immediate 1))))

(setf (special-operator 'sicl-system:wlogshift)
      (lambda (arguments environment)
	(assert (= (length arguments) 2))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  ,@(compile-form (cadr arguments) environment)
	  (push-from-arg-immediate 0)
	  (log-shift)
	  (pop-to-arg-immediate 0))))

(setf (special-operator 'sicl-system:warshift)
      (lambda (arguments environment)
	(assert (= (length arguments) 2))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  ,@(compile-form (cadr arguments) environment)
	  (push-from-arg-immediate 0)
	  (ar-shift)
	  (pop-to-arg-immediate 0))))

(setf (special-operator 'sicl-system:wand)
      (lambda (arguments environment)
	(assert (= (length arguments) 2))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  ,@(compile-form (cadr arguments) environment)
	  (push-from-arg-immediate 0)
	  (log-and)
	  (pop-to-arg-immediate 0))))

(setf (special-operator 'sicl-system:wior)
      (lambda (arguments environment)
	(assert (= (length arguments) 2))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  ,@(compile-form (cadr arguments) environment)
	  (push-from-arg-immediate 0)
	  (log-ior)
	  (pop-to-arg-immediate 0))))

(setf (special-operator 'sicl-system:wnot)
      (lambda (arguments environment)
	(assert (= (length arguments) 1))
	`(,@(compile-form (car arguments) environment)
	  (push-from-arg-immediate 0)
	  (log-not)
	  (pop-to-arg-immediate 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Special operators from the Common Lisp HyperSpec.

(setf (special-operator 'scl:quote)
      (lambda (arguments environment)
	(declare (ignore environment))
	`((push-constant ',(car arguments))
	  (pop-to-arg-immediate 0))))

;;; *sigh* when you write 'stuff in code, the host 
;;; reader returns (cl:quote stuff)
(setf (special-operator 'quote)
      (lambda (arguments environment)
	(declare (ignore environment))
	`((push-constant ',(car arguments))
	  (pop-to-arg-immediate 0))))

;;; FIXME: Handle IFs with no "else" branch. 
(setf (special-operator 'scl:if)
      (lambda (arguments environment)
	(let ((test-code (compile-form (car arguments) environment))
	      (then-code (compile-form (cadr arguments) environment))
	      (else-code (compile-form (caddr arguments) environment))
	      (else-label (gensym))
	      (out-label (gensym)))
	  ;; The final code will have the following form:
	  ;; <test> (branch-nil else) <then> (branch out) <else> out
	  `((comment "special operator IF")
	    (comment "code for test of IF")
	    ,@test-code
	    (branch-nil ,else-label)
	    (comment "code for `then' branch")
	    ,@then-code
	    (branch ,out-label)
	    (comment "code for `else' branch")
	    ,else-label
	    ,@else-code
	    ,out-label))))

(setf (special-operator 'scl:progn)
      (lambda (arguments environment)
	(loop for form in arguments
	      append (compile-form form environment))))

(setf (special-operator 'scl:let)
      (lambda (arguments environment)
	(let ((bindings (car arguments))
	      (body (cdr arguments))
	      (code '((comment "special operator LET"))))
	  ;; Compile the initialization forms in the same environment
	  ;; as that of the LET form itself.  The values of these
	  ;; forms are temporarily stored on the operand stack.
	  (setf code
		(append code
			`(,@(loop for form in (mapcar #'cadr bindings)
				  append `(,@(compile-form form environment)
					   (push-from-arg-immediate 0))))))
	  ;; Pop the temporaries from the operand stack and put them
	  ;; in the arguments to perpare for the call to the
	  ;; subroutine represented by the LET body.
	  (setf code
		(append code
			`(,@(loop for i downfrom (1- (length bindings)) to 0
				  collect `(pop-to-arg-immediate ,i))
			    (push-immediate ,(length bindings))
			    (pop-to-arg-count))))
	  ;; Augment the compilation environment with the bindings for the
	  ;; new variables. 
	  (loop for name in (mapcar #'car bindings)
		do (unless (eq (variable-information name environment)
			       :special)
		     (setf environment
			   (make-instance 'lexical-variable-environment
					  :identifier name
					  :parent environment
					  :declarations '()))))
	  ;; Compile the code for the body in the augmented compilation
	  ;; environment. 
	  (let ((body-code (compile-form (cons 'scl:progn body) environment))
		(subroutine-label (gensym))
		(out-label (gensym)))
	    ;; We are building a subroutine out of the LET body.  This
	    ;; subroutine needs to create its lexical environment, and
	    ;; initialize the lexical environment from the argument,
	    ;; before it starts executing the body code.  The complete
	    ;; subroutine code is then added as a constant to the
	    ;; surrounding code.
	    (append code
		    `((call-subroutine ,subroutine-label)
		      (branch ,out-label)
		      ,subroutine-label
		      ,@(loop for name in (mapcar #'car bindings)
			      for arg from 0
			      append
			      (if (eq (variable-information name environment)
				      :special)
				  `((push-constant ',name)
				    (push-from-arg-immediate ,arg)
				    (bind-special))
				  `((bind 1)
				    (push-from-arg-immediate ,arg)
				    (pop-to-lexical 0 0))))
		      ,@body-code
		      (subroutine-epilogue)
		      ,out-label))))))

(setf (special-operator 'scl:function)
      (lambda (arguments environment)
	(let ((fun (car arguments)))
	  (cond ((and (consp fun) (eq (car fun) 'scl:lambda))
		 ;; Only handle lambda lists with required parameters for
		 ;; now.
		 ;;
		 ;; Augment the compilation environment with the bindings
		 ;; for the new variables.
		 (let ((parameters (cadr fun))
		       (body (cddr fun))
		       (code '((comment "special operator FUNCTION"))))
		   (loop for name in parameters
			 do (unless (eq (variable-information name environment)
					:special)
			      (setf environment
				    (make-instance 'lexical-variable-environment
						   :identifier name
						   :parent environment
						   :declarations '()))))
		   ;; Compile the code for the body in the augmented
		   ;; compilation environment.
		   (let ((body-code (compile-form (cons 'scl:progn body) environment))
			 (subroutine-label (gensym))
			 (out-label (gensym)))
		     ;; We are building a subroutine out of the LAMBDA
		     ;; body.  This subroutine needs to create its
		     ;; lexical environment, and initialize the lexical
		     ;; environment from the argument, before it starts
		     ;; executing the body code.
		     (append code
			     `((enclose ,subroutine-label)
			       (pop-to-arg-immediate 0)
			       (push-immediate 1)
			       (pop-to-arg-count)
			       (branch ,out-label)
			       ,subroutine-label
			       (function-prologue)
			       ,@(loop for name in parameters
				       for arg from 0
				       append
				       (if (eq (variable-information name environment)
					       :special)
					   `((push-constant ',name)
					     (push-from-arg-immediate ,arg)
					     ;; FIXME: wrong.
					     (bind-special))
					   `((bind 1)
					     (push-from-arg-immediate ,arg)
					     (pop-to-lexical 0 0))))
			       ,@body-code
			       (function-epilogue)
			       ,out-label)))))
		((consp fun)
		 ;; FIXME: it must be of the form (setf symbol) but check it.
		 ;; FIXME: search the environment for function information.
		 ;; For a form like (function (setf sym)), we
		 ;; directly access the symbol-setf-function slot
		 ;; of the symbol.
		 `((comment "special operator FUNCTION")
		   (comment ,(format nil
				     "with argument (setf ~s)"
				     (cadr fun)))
		   (comment ,(format nil "symbol ~s"
				     (cadr fun)))
		   (push-constant ',(cadr fun))
		   (comment "A symbol is a standard instance.  Find the slot vector.")
		   (remove-tag)
		   (access-element-untagged 1)
		   (comment "The setf-function slot is number 5.")
		   (access-element-untagged 5)
		   (pop-to-arg-immediate 0)
		   (push-immediate 1)
		   (pop-to-arg-count)))
		(t
		 ;; FIXME: search the environment for function information.
		 `((push-constant 'scl:symbol-function)
		   (pop-to-arg-immediate 0)
		   (push-constant ',fun)
		   (pop-to-arg-immediate 1)
		   (push-immediate 2)
		   (pop-to-arg-count)
		   (call)))))))

;; (setf (special-operator 'unwind-protect)
;;       (lambda (arguments environment)
;; 	(let ((code '())
;; 	      (constants '())
;; 	      (protected-code-label (gensym))
;; 	      (cleanup-code-label (gensym)))
;; 	  (destructuring-bind (protected-form &rest cleanup-forms)
;; 	      arguments
;; 	    (let* ((protected-code (compile-form protected-form environment))
;; 		   (final-protected-code
;; 		     (append `((push-from-arg-immediate 0) ; cleanup thunk
;; 			       (dup)
;; 			       (protect))
;; 			     protected-code
;; 			     ;; FIXME: need to save and restore values
;; 			     ;; of protected form.
;; 			     `((call-function))))) ; call cleanup thunk
;; 		(setf constants
;; 		      (append constants
;; 			      `((,protected-code-label
;; 				 ,final-protected-code code))
;; 			      protected-constants))))
;; 	    (multiple-value-bind (cleanup-code cleanup-constants)
;; 		(compile-form (cons 'progn cleanup-forms) environment)
;; 	      (setf constants
;; 		    (append constants
;; 			    `((,cleanup-code-label ,cleanup-code code))
;; 			    cleanup-constants))
;; 	      (setf code
;; 		    `((push-constant ,cleanup-code-label)
;; 		      ;; FIXME: enclose takes parameters now. 
;; 		      (enclose)
;; 		      (pop-to-arg-immediate 0)
;; 		      (push-constant ,protected-code-label)
;; 		      (call)))
;; 	      (values code constants)))))

(setf (special-operator 'scl:eval-when)
      (lambda (arguments environment)
	(when (member :compile-toplevel (car arguments))
	  (eval (cons 'progn (cdr arguments))))
;;	  (eval (cons 'scl:progn (cdr arguments))))
	(if (member :load-toplevel (car arguments))
	    (compile-form (cons 'scl:progn (cdr arguments)) environment)
	    '())))

(setf (special-operator 'scl:block)
      (lambda (arguments environment)
	(let ((name (car arguments))
	      (body (cdr arguments)))
	  (let ((new-env (make-instance 'block-environment
					:identifier name
					:parent environment
					:declarations '()))
		(subroutine-label (gensym))
		(out-label (gensym)))
	    `((comment "The BLOCK body is called as a subroutine")
	      (comment "with the call stack as an argument")
	      (push-from-call-stack)
	      ;; Do we need to go through the arguments?
	      ;; Maybe a subroutine call changes the call stack?
	      (pop-to-arg-immediate 0)
	      (call-subroutine ,subroutine-label)
	      (branch ,out-label)
	      ,subroutine-label
	      (comment "BLOCK body as subroutine.")
	      (subroutine-prologue)
	      (comment "The subroutine stores the call stack")
	      (comment "in a lexical variable")
	      (bind 1)
	      (push-from-arg-immediate 0)
	      (pop-to-lexical 0 0)
	      (comment "code for body of BLOCK")
	      ,@(compile-form (cons 'scl:progn body) new-env)
	      (subroutine-epilogue)
	      ,out-label)))))
