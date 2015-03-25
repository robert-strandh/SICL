(cl:in-package #:sicl-cross-compiler)

(defclass environment () ())

(defclass global-environment (environment)
  ((%macros :initform (make-hash-table :test #'eq) :reader macros)
   (%functions :initform (make-hash-table :test #'eq) :reader functions)
   (%special-operators :initform (make-hash-table :test #'eq) :reader special-operators)
   (%constants :initform (make-hash-table :test #'eq) :reader constants)
   (%specials :initform (make-hash-table :test #'eq) :reader specials)))

(defparameter *compiler-environment*
  (make-instance 'global-environment))

;;; Enter a special for testing purposes.
(setf (gethash '*print-base* (specials *compiler-environment*)) t)

(defclass delta-environment (environment)
  ((%parent :initarg :parent :reader parent)
   (%identifier :initarg :identifier :reader identifier)
   (%declarations :initarg :declarations :reader declarations)))

(defclass variable-environment (delta-environment) ())
(defclass function-environment (delta-environment) ())

(defclass macro-environment (function-environment)
  ((%expander :initarg expander :reader expander)))

(defclass binding-environment-mixin () ())

(defun find-binding (identifier environment type)
  (labels ((aux (environment level)
	     (cond ((typep environment 'global-environment)
		    nil)
		   ((and (typep environment type)
			 (eq identifier (identifier environment)))
		    level)
		   ((typep environment 'binding-environment-mixin)
		    (aux (parent environment) (1+ level)))
		   (t (aux (parent environment) level)))))
    (aux environment 0)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variable environment

(defclass lexical-variable-environment
    (variable-environment binding-environment-mixin)
  ())

(defgeneric variable-information-using-class (identifier env))

(defmethod variable-information-using-class (identifier env)
  (variable-information-using-class identifier (parent env)))

(defmethod variable-information-using-class (identifier (env variable-environment))
  (values :lexical env '()))

;;; FIXME: Clearly wrong.
(defmethod variable-information-using-class (identifier (env global-environment))
  (let ((constant (gethash identifier (constants env))))
    (if (not (null constant))
	(values :constant nil '((type . t)))
	(let ((special (gethash identifier (specials env))))
	  (if (not (null special))
	      (values :special nil '((type . t)))
	      (values nil nil nil))))))

(defun variable-information (identifier &optional env)
  (variable-information-using-class identifier (or env *compiler-environment*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function environment

(defclass lexical-function-environment
    (function-environment binding-environment-mixin)
  ())

(defgeneric function-information-using-class (identifier env))

(defmethod function-information-using-class (identifier env)
  (function-information-using-class identifier (parent env)))

(defmethod function-information-using-class (identifier (env function-environment))
  (values :function env '()))

(defmethod function-information-using-class (identifier (env macro-environment))
  (values :macro env '()))

;;; FIXME: Clearly wrong.
(defmethod function-information-using-class (identifier (env global-environment))
  (let ((macro (gethash identifier (macros env))))
    (if (not (null macro))
	(values :macro nil '((type . t)))
	(let ((function (gethash identifier (functions env))))
	  (if (not (null function))
	      (values :function nil '((type . t)))
	      (let ((special-form (gethash identifier (special-operators env))))
		(if (not (null special-form))
		    (values :special-form nil '((type . t)))
		    (values nil nil nil))))))))
  
(defun function-information (identifier &optional env)
  (function-information-using-class identifier (or env *compiler-environment*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Block environment

(defclass block-environment (delta-environment binding-environment-mixin) ())

(defgeneric block-information-using-class (identifier env))

(defmethod block-information-using-class (identifier env)
  (block-information-using-class identifier (parent env)))

(defmethod block-information-using-class (identifier (env block-environment))
  (values :block env '()))

(defmethod block-information-using-class (identifier (env global-environment))
  (values nil nil ()))

(defun block-information (identifier &optional env)
  (block-information-using-class identifier (or env *compiler-environment*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The MACRO-FUNCTION and (SETF MACRO-FUNCTION) functions. 
;;;
;;; The MACRO-FUNCTION symbol is shadowed in the cross compiler
;;; package.
;;;
;;; Using (SETF (MACRO-FUNCTION <sym>) <function>) with this symbol,
;;; causes a host function to be associated with a symbol and this
;;; association is done in the target environment.
;;;
;;; Using (MACRO-FUNCTION <sym> <env>) searches the environment chain 
;;; for a macro function associated with the symbol <sym>.

(defgeneric macro-function-using-class (symbol environment))

(defmethod macro-function-using-class (symbol (environment environment))
  (macro-function-using-class symbol (parent environment)))

(defmethod macro-function-using-class (symbol (environment global-environment))
  (values (gethash symbol (macros environment))))

(defmethod macro-function-using-class (symbol (environment macro-environment))
  (if (eq symbol (identifier environment))
      (expander environment)
      (macro-function-using-class symbol (parent environment))))

(defun macro-function (symbol &optional environment)
  (macro-function-using-class symbol (or environment *compiler-environment*)))

(defun (setf macro-function) (new-function name &optional environment)
  ;; The environment must be the null lexical environment
  (assert (null environment))
  (setf (gethash name (macros *compiler-environment*))
	new-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The SPECIAL-OPERATOR and (SETF SPECIAL-OPERATOR) functions. 
;;;
;;; We use these to associate special compilers with special operators
;;; using traditional data-driven programming.

(defgeneric special-operator-using-class (symbol environment))

(defmethod special-operator-using-class (symbol environment)
  (special-operator-using-class symbol (parent environment)))

(defmethod special-operator-using-class (symbol (environment global-environment))
  (values (gethash symbol (special-operators environment))))

(defun special-operator (symbol &optional environment)
  (special-operator-using-class symbol (or environment *compiler-environment*)))

(defun (setf special-operator) (compiler symbol &optional environment)
  ;; The environment must be the null lexical environment
  (assert (null environment))
  (setf (gethash symbol (special-operators *compiler-environment*))
	compiler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The DEFMACRO macro.
;;;
;;; We define a macro associated with the symbol DEFMACRO in the cross
;;; compiler package.  It is used just like DEFMACRO macro in the host
;;; environment, but it defines macro functions in the target
;;; environment instead.

;;; FIXME: handle &whole.
(cl:defmacro defmacro (name lambda-list &body body)
  (let ((pos (position '&environment lambda-list))
	(form-var (gensym))
	(name-var (gensym))
	(env-var (gensym)))
    `(setf (macro-function ',name)
	   ,(if (null pos)
		`(lambda (,form-var ,env-var)
		   (declare (ignore ,env-var))
		   ,(if (eq (car lambda-list) '&whole)
			`(destructuring-bind ,(list*
					       (car lambda-list)
					       (cadr lambda-list)
					       name-var
					       (cddr lambda-list))
			     ,form-var
			   ,@body)
			`(destructuring-bind ,lambda-list
			     (cdr ,form-var)
			   ,@body)))
		(let ((new-lambda-list
			(append (subseq lambda-list 0 pos)
				(subseq lambda-list (+ pos 2)))))
		  `(lambda (,form-var ,(nth (1+ pos) lambda-list))
		     ,(if (eq (car new-lambda-list) '&whole)
			  `(destructuring-bind ,(list*
						 (car new-lambda-list)
						 (cadr new-lambda-list)
						 name-var
						 (cddr new-lambda-list))
			       ,form-var
			     ,@body)
			  `(destructuring-bind ,new-lambda-list
			       (cdr ,form-var)
			     ,@body))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The MACROEXPAND-1 and MACROEXPAND functions. 

;;; FIXME: handle symbol macros
(defun macroexpand-1 (form &optional environment)
  (when (null environment)
    (setf environment *compiler-environment*))
  (if (consp form)
      (let ((expander (macro-function (car form) environment)))
	(if (null expander)
	    (values form nil)
	    (values (funcall expander form environment) t)))
      ;; FIXME: handle symbol macros
      (values form nil)))

(defun macroexpand (form &optional environment)
  (multiple-value-bind (new-form expanded-p)
      (macroexpand-1 form environment)
    (if expanded-p
	(values (macroexpand new-form environment) t)
	(values form nil))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define SCL:DEFMACRO.

(defmacro scl:defmacro (name lambda-list &body body)
  `(scl:eval-when (:compile-toplevel)
     (defmacro
	 ,name
	 ,(sublis '((scl:&optional . cl:&optional)
		    (scl:&rest . cl:&rest)
		    (scl:&body . cl:&body)
		    (scl:&key . cl:&key)
		    (scl:&allow-other-keys . cl:&allow-other-keys)
		    (scl:&aux . cl:&aux)
		    (scl:&environment . cl:&environment)
		    (scl:&whole . cl:&whole))
	   lambda-list)
       ,@body)))

(defmacro scl:when (test &body body)
  `(scl:if ,test
	   (scl:progn ,@body)
	   scl:nil))

(defmacro scl:unless (test &body body)
  `(scl:if ,test
	   (scl:progn ,@body)
	   (scl:nil)))

(defmacro scl:lambda (parameters &body body)
  `(scl:function (scl:lambda ,parameters ,@body)))

(defmacro scl:setf (place value)
  (if (symbolp place)
      `(scl:setq ,place ,value)
      `(scl:funcall (scl:function (scl:setf ,(car place)))
		    ,value ,(cadr place))))

;; (defmacro scl:defun (name lambda-list &body body)
;;   `(scl:setf (scl:fdefinition ',name)
;; 	     (scl:lambda ,lambda-list
;; 	       (scl:block ,name
;; 		 ,@body))))
