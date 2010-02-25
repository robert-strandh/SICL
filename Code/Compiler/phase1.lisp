;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printer programming

(defclass compiler-object () ())

(defmethod print-object ((obj compiler-object) stream)
  (pprint-logical-block (stream nil :prefix "[" :suffix "]")
    (format stream "~s ~2i" (class-name (class-of obj)))
    (loop for slot in (sb-mop:class-slots (class-of obj))
	  do (format stream "~_~s ~W "
		     (car (sb-mop:slot-definition-initargs slot))
		     (if (slot-boundp obj (sb-mop:slot-definition-name slot))
			 (slot-value obj (sb-mop:slot-definition-name slot))
			 "no-value")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal representation of source code

(defclass form (compiler-object)
  ((%value-count :initarg :value-count :reader value-count)))

(defclass variable-form (form)
  ((%name :initarg :name :initform nil :reader name)))

(defun make-variable-form (name)
  (make-instance 'variable-form
		 :value-count 1
		 :name name))

(defclass constant-form (form)
  ((%value :initarg :value :reader value)))

(defun make-constant-form (value)
  (make-instance 'constant-form
		 :value-count 1
		 :value value))

(defclass function-call-form (form)
  ((%function-form :initarg :function-form :reader function-form)
   (%arguments :initarg :arguments :reader arguments)))

(defun make-function-call-form (function-form arguments)
  (make-instance 'function-call-form
		 :value-count :unknown
    :function-form function-form
    :arguments arguments))

(defclass progn-form (form)
  ((%body-forms :initarg :body-forms :reader body-forms)))

(defun make-progn-form (body-forms)
  (make-instance 'progn-form
		 :value-count (value-count (car (last body-forms)))
		 :body-forms body-forms))

(defclass binding-form (compiler-object)
  ((%binding :initarg :binding :reader binding)
   (%value :initarg :value :reader value)))

(defclass let-form (form)
  ((%binding-forms :initarg :binding-forms :reader binding-forms)
   (%body-form :initarg :body-form :reader body-form)))

(defun make-let-form (binding-forms body-form)
  (make-instance 'let-form
		 :value-count (value-count body-form)
		 :binding-forms binding-forms
		 :body-form body-form))

(defclass setq-form (form)
  ((%binding :initarg :binding :reader binding)
   (%value :initarg :value :reader value)))

(defun make-setq-form (binding value)
  (make-instance 'setq-form
		 :value-count (value-count value)
		 :binding binding :value value))

(defclass if-form (form)
  ((%test-form :initarg :test-form :reader test-form)
   (%then-form :initarg :then-form :reader then-form)
   (%else-form :initarg :else-form :reader else-form)))

(defun make-if-form (test-form then-form else-form)
  (make-instance 'if-form
		 :value-count (if (eql (value-count then-form) (value-count else-form))
				  (value-count then-form)
				  :unknown)
		 :test-form test-form
		 :then-form then-form
		 :else-form else-form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment

(defun make-binding-form (binding value)
  (make-instance 'binding-form :binding binding :value value))

(defclass namespace (compiler-object)
  ((%bindings :initarg :bindings :initform '() :accessor bindings)))

(defgeneric copy-namespace (namespace))

(defmethod copy-namespace ((namespace namespace))
  (make-instance 'namespace :bindings (bindings namespace)))

(defgeneric lookup (name namespace))

(defmethod lookup (name (namespace namespace))
  (find name (bindings namespace) :test #'eq :key #'name))

(defgeneric add-binding (binding namespace))

(defmethod add-binding (binding (namespace namespace))
  (push binding (bindings namespace)))

(defclass environment (compiler-object)
  ((%lexical :initarg :lexical :initform (make-instance 'namespace) :accessor lexical)))

(defgeneric copy-environment (environment))

(defmethod copy-environment ((environment environment))
  (make-instance 'environment
		 :lexical (copy-namespace (lexical environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convert source code into internal form

(defgeneric convert (form environment))

(defmethod convert (form environment)
  (make-constant-form form))

(defmethod convert ((form symbol) environment)
  (if (or (member form '(nil t))
	  (eq (symbol-package form) (find-package :keyword)))
      (make-constant-form form)
      (let ((binding (lookup form (lexical environment))))
	(assert (not (null binding)))
	binding)))

(defgeneric convert-special (operator form environment))

(defmethod convert ((form cons) environment)
  (let ((expanded-form (macroexpand form)))
    (if (special-operator-p (car expanded-form))
	(convert-special (car expanded-form) form environment)
	(make-function-call-form
	 (car form)
	 (mapcar (lambda (argument-form)
		   (convert argument-form environment))
		 (cdr expanded-form))))))

(defun proper-list-p (list)
  (null (last list 0)))

(defmethod convert-special ((op (eql 'setq)) form environment)
  (assert (proper-list-p form))
  (assert (= (length form) 3))
  (destructuring-bind (var val) (cdr form)
    (assert (symbolp var))
    (make-setq-form (convert var environment)
		    (convert val environment))))

(defun convert-body (forms environment)
  (assert (proper-list-p forms))
  (cond ((null forms)
	 (make-constant-form nil))
	((null (cdr forms))
	 (convert (car forms) environment))
	(t (make-progn-form (loop for form in forms
				  collect (convert form environment))))))

(defmethod convert-special ((op (eql 'progn)) form environment)
  (convert-body (cdr form) environment))

(defmethod convert-special ((op (eql 'let)) form environment)
  (assert (proper-list-p form))
  (assert (>= (length form) 2))
  (destructuring-bind (binding-forms &rest body-forms) (cdr form)
    (assert (proper-list-p binding-forms))
    (assert (proper-list-p body-forms))
    (let* ((binding-forms
	    (loop for binding-form in binding-forms
		  do (assert (or (symbolp binding-form)
				 (and (proper-list-p binding-form)
				      (= (length binding-form) 2)
				      (symbolp (car binding-form)))))
		  collect (make-binding-form
			   (make-variable-form (if (symbolp binding-form)
						   binding-form
						   (car binding-form)))
			   (convert (if (symbolp binding-form)
					nil
					(cadr binding-form))
				    environment))))
	   (new-env (let ((new-env (copy-environment environment)))
		      (loop for binding-form in binding-forms
			    do (add-binding (binding binding-form)
					    (lexical new-env)))
		      new-env)))
      (make-let-form binding-forms
		     (convert-body body-forms new-env)))))

(defmethod convert-special ((op (eql 'quote)) form environment)
  (assert (proper-list-p form))
  (assert (= (length form) 2))
  (make-constant-form (cadr form)))

(defmethod convert-special ((op (eql 'if)) form environment)
  (assert (proper-list-p form))
  (assert (<= 3 (length form) 4))
  (make-if-form (convert (cadr form) environment)
		(convert (caddr form) environment)
		(if (null (cdddr form))
		    (make-instance 'constant-form :value nil)
		    (convert (cadddr form) environment))))

(defgeneric normalize (form))

(defmethod normalize (form)
  form)

(defun bindings-or-nil (form)
  (if (typep form 'let-form) (bindings form) '()))

(defun body-form-or-form (form)
  (if (typep form 'let-form) (body-form form) form))

(defun mappend (fun &rest lists)
  (mapcan #'copy-list
	  (apply #'mapcar fun lists)))

(defmethod normalize ((form if-form))
  (let ((normalized-test-form (normalize (test-form form)))
	(normalized-then-form (normalize (then-form form)))
	(normalized-else-form (normalize (else-form form))))
    (if (and (not (typep normalized-test-form 'let-form))
	     (not (typep normalized-then-form 'let-form))
	     (not (typep normalized-else-form 'let-form)))
	(make-instance 'if-form
	  :test-form normalized-test-form
	  :then-form normalized-then-form
	  :else-form normalized-else-form)
	(make-instance 'let-form
	  :bindings (append (bindings-or-nil normalized-test-form)
			    (bindings-or-nil normalized-then-form)
			    (bindings-or-nil normalized-else-form))
	  :body-form (make-instance 'if-form
		       :test-form (body-form-or-form normalized-test-form)
		       :then-form (body-form-or-form normalized-then-form)
		       :else-form (body-form-or-form normalized-else-form))))))

(defun body-forms-or-list-of-form (form)
  (if (typep form 'progn-form) (body-forms form) (list form)))

;; (defmethod normalize ((form progn-form))
;;   (let* ((normalized-body-forms (mapcar #'normalize (body-forms form)))
;; 	 (bindings (mappend #'bindings-or-nil normalized-body-forms)))
;;     (if (null bindings)
;; 	(make-instance 'progn-form
;; 	  :body-forms normalized-body-forms)
;; 	(let ((let-free-body-forms (mapcar #'body-form-or-form normalized-body-forms))
;; 	      (new-body-forms (mappend #'body-forms-or-list-form let-free-body-form)))
;; 	  (make-instance 'let-form
;; 	    :bindings bindings
;; 	    :body-form (make-instance 'progn-form
;; 			 :body-forms new-body-forms))))))

(defmethod normalize ((form let-form))
  (let ((normalized-body-form (normalize (body-form form))))
    (if (typep normalized-body-form 'let-form)
	(make-instance 'let-form
	  :bindings (append (bindings form) (bindings normalized-body-form))
	  :body-form (body-form normalized-body-form))
	(make-instance 'let-form
	  :bindings (bindings form)
	  :body-form normalized-body-form))))

;; (defmethod normalize ((form function-call-form))
;;   (let ((function-reference (function-reference form))
;; 	(normalized-arguments (mapcar #'normalize (arguments form)))
;; 	(bindings (mappend #'bindings-or-nil normalized-arguments)))
;;     (if (null bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graph

(defclass instruction () ())

(defclass single-successor-instruction (instruction)
  ((%successor :initarg :successor :accessor successor)))

(defclass copy-instruction (single-successor-instruction)
  ((%destination :initarg :destination :accessor destination)
   (%source :initarg :source :accessor source)))

(defclass load-instruction (single-successor-instruction)
  ((%destination :initarg :destination :accessor destination)
   (%value :initarg :value :accessor value)))

(defclass test-instruction (instruction)
  ((%place :initarg :place :accessor place)
   (%true-successor :initarg :true-successor :accessor true-successor)
   (%false-successor :initarg :false-successor :accessor false-successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile and expression in internal form

(defgeneric compile-expression (form values successors))

;;; factor the duplicated code
(defmethod compile-expression ((form constant-form) values successors)
  (if (null values)
      (warn "constant form compiled in a context where its value is not needed")
      (let ((succ successors))
	(loop for place in (cdr values)
	      do (setf succ (make-instance 'load-instruction
			      :destination place
			      :value nil
			      :successor succ)))
	(make-instance 'load-instruction
	  :destination (car values)
	  :value form
	  :successor succ))))

;;; factor the duplicated code
(defmethod compile-expression ((form variable-form) values successors)
  (if (null values)
      (warn "variable form compiled in a context where its value is not needed")
      (let ((succ successors))
	(loop for place in (cdr values)
	      do (setf succ (make-instance 'load-instruction
			      :destination place
			      :value nil
			      :successor succ)))
	(make-instance 'copy-instruction
	  :destination (car values)
	  :value form
	  :successor succ))))
