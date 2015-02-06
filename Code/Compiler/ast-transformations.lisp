(cl:in-package #:sicl-ast-transformations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The purpose of this backend-specific transformation is to replace
;;; small integer and character constants with immediates.  It depends
;;; on the backend because different backends can handle constants of
;;; different magnitudes, and different backends might represent small
;;; integers and characters differently. 

(defgeneric introduce-immediates (ast backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For each external reference (constant, global function, global
;;; value), enter it into an object graph.
;;; FIXME: say more...

(defparameter *argument-functions*
  '(make-string (setf schar) fdefinition intern make-symbol))

(defparameter *object-graph* '())

(defclass item ()
  ((%object :initarg :object :reader object)
   (%lexical-location :initarg :lexical-location :accessor lexical-location)
   (%offset :initarg :offset :accessor offset)))

(defun find-item (object)
  (find object *object-graph* :key #'object :test #'equal))

(defun ensure-object (object &optional offset)
  (let ((item (find-item object)))
    (if (null item)
	(progn 
	  (cond ((stringp object)
		 (ensure-object '(function make-string))
		 (ensure-object '(function (setf schar))))
		((symbolp object)
		 (ensure-object (symbol-name object))
		 (let ((package (symbol-package object)))
		   (if (null package)
		       (ensure-object '(function make-symbol))
		       (progn 
			 (ensure-object (package-name package))
			 (ensure-object '(function intern))))))
		((consp object)
		 (cond ((eq (car object) 'function)
			(unless (member (cadr object) *argument-functions*
					:test #'equal)
			  (ensure-object (cadr object))
			  (ensure-object '(function fdefinition))))
		       ((eq (car object) 'function-cell)
			(ensure-object (cadr object))
			(ensure-object
			 '(function sicl-global-environment:find-function-cell)))
		       ((eq (car object) 'value-cell)
			(ensure-object (cadr object))
			(ensure-object
			 '(function sicl-global-environment:find-value-cell)))
		       (t
			(ensure-object (car object))
			(ensure-object (cdr object))
			(ensure-object '(function cons)))))
		(t
		 (error "unknown object")))
	  (push (make-instance 'item :object object :offset offset)
		*object-graph*))
	(unless (null offset)
	  (setf (offset item) offset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find all externals in an AST.

(defun find-externals (ast)
  (let ((table (make-hash-table :test #'eq)))
    (labels
	((traverse (ast in-function-p)
	   (unless (gethash ast table)
	     (setf (gethash ast table) t)
	     (typecase ast
	       (sicl-ast:constant-ast
		(ensure-object (sicl-ast:value ast) in-function-p))
	       (sicl-ast:global-ast
		(if in-function-p
		    (ensure-object `(function-cell ,(sicl-ast:name ast)) t)
		    (ensure-object `(function ,(sicl-ast:name ast)) nil)))
	       (sicl-ast:special-ast
		(if in-function-p
		    (ensure-object `(value-cell ,(sicl-ast:name ast)) t)
		    (ensure-object `(symbol-value ,(sicl-ast:name ast)) nil)))
	       (t
		nil))
	     (loop for child in (sicl-ast:children ast)
		   do (traverse child
				(or in-function-p
				    (typep ast 'sicl-ast:function-ast)))))))
      (traverse ast nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Push the argument functions to the end of the object graph.

(defun push-argument-functions ()
  (loop for name in *argument-functions*
	for fun = `(function ,name)
	do (let ((item (find-item fun)))
	     (unless (null item)
	       (setf *object-graph*
		     (append (remove item *object-graph* :test #'eq)
			     (list item)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assign a lexical location to each external item.

(defun assign-lexical-locations (items)
  (loop for item in items
	do (setf (lexical-location item)
		 (sicl-ast:make-lexical-ast (gensym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assign an offset in the linkage vector to each item that needs
;;; one.

(defun assign-linkage-vector-offsets (items)
  (loop ;; Start the offset at 1 so as to leave room for the code
	;; object in offset 0.  Encode the offset as a fixnum. 
	with offset = 4
	for item in items
	do (unless (null (offset item))
	     (setf (offset item) offset)
	     (incf offset 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From an item, create an AST that computes the object and stores it
;;; in the lexical location associated with the item.

(defun find-lexical-location (object)
  (lexical-location (find-item object)))

(defun create-ast (item)
  (let ((object (object item)))
    (cond ((stringp object)
	   (sicl-ast:make-progn-ast
	    (cons (sicl-ast:make-setq-ast
		   (lexical-location item)
		   (sicl-ast:make-call-ast
		    (find-lexical-location '(function make-string))
		    (list 
		     (sicl-ast:make-immediate-ast (* 4 (length object))))))
		  (loop for char across object
			for index from 0
			collect (sicl-ast:make-call-ast
				 (find-lexical-location '(function (setf schar)))
				 (list
				  (+ (* 4 (char-code char)) 2)
				  (* 4 index)))))))
	  ((symbolp object)
	   (sicl-ast:make-setq-ast
	    (lexical-location item)
	    (let ((package (symbol-package object)))
	      (if (null package)
		  (sicl-ast:make-call-ast
		   (find-lexical-location '(function make-symbol))
		   (list
		    (find-lexical-location (symbol-name object))))
		  (sicl-ast:make-call-ast
		   (find-lexical-location '(function intern))
		   (list
		    (find-lexical-location (symbol-name object))
		    (find-lexical-location (package-name package))))))))
	  ((consp object)
	   (sicl-ast:make-setq-ast
	    (lexical-location item)
	    (cond ((eq (car object) 'function)
		   (if (member (cadr object) *argument-functions*
			       :test #'equal)
		       (sicl-ast:make-arg-ast
			(sicl-ast:make-immediate-ast
			 (* 4 (position (cadr object) *argument-functions*
					:test #'equal))))
		       (sicl-ast:make-call-ast
			(find-lexical-location '(function fdefinition))
			(list
			 (find-lexical-location (cadr object))))))
		 ((eq (car object) 'function-cell)
		  (sicl-ast:make-call-ast
		   (find-lexical-location
		    '(function sicl-global-environment:find-function-cell))
		   (list
		    (find-lexical-location (cadr object)))))
		 ((eq (car object) 'value-cell)
		  (sicl-ast:make-call-ast
		   (find-lexical-location
		    '(function sicl-global-environment:find-value-cell))
		   (list
		    (find-lexical-location (cadr object)))))
		 (t
		  (sicl-ast:make-call-ast
		   (find-lexical-location '(function cons))
		   (list
		    (find-lexical-location (car object))
		    (find-lexical-location (cdr object))))))))
	  (t
	   (error "unknown object")))))
	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Replace instances of CONSTANT-AST and GLOBAL-AST.  If the
;;; instances are inside a FUNCTION-AST, then replace them by
;;; instances of LOAD-CONSTANT-AST and LOAD-GLOBAL-AST.  If they are
;;; outside a FUNCTION-AST, then replace them by the corresponding
;;; LEXICAL-AST.

(defun find-offset (object)
  (offset (find-item object)))

(defun replace-externals (ast)
  (let ((table (make-hash-table :test #'eq)))
    (labels
	((traverse (ast in-function-p)
	   (unless (gethash ast table)
	     (setf (gethash ast table) t)
	     (when (typep ast 'sicl-ast:function-ast)
	       (setf in-function-p t))
	     (loop for rest on (sicl-ast:children ast)
		   for child = (car rest)
		   do (typecase child
			(sicl-ast:constant-ast
			 (setf (car rest)
			       (if in-function-p
				   (sicl-ast:make-load-constant-ast
				    (find-offset (sicl-ast:value child)))
				   (find-lexical-location
				    (sicl-ast:value child)))))
			(sicl-ast:global-ast
			 (setf (car rest)
			       (if in-function-p
				   (sicl-ast:make-load-global-ast
				    (find-offset
				     `(function-cell ,(sicl-ast:name child))))
				   (find-lexical-location
				    `(function ,(sicl-ast:name child))))))
			(sicl-ast:special-ast
			 (setf (car rest)
			       (if in-function-p
				   (sicl-ast:make-load-global-ast
				    (find-offset
				     `(value-cell ,(sicl-ast:name child))))
				   (find-lexical-location
				    `(symbol-value ,(sicl-ast:name child))))))
			(t
			 nil)))
	     (loop for child in (sicl-ast:children ast)
		   do (traverse child in-function-p)))))
      (traverse ast nil))))
