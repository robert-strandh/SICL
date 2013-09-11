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

(defun ensure-object (object &optional offset)
  (let ((item (find object *object-graph* :key #'object :test #'equal)))
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
			(ensure-object '(function find-function-cell)))
		       ((eq (car object) 'value-cell)
			(ensure-object (cadr object))
			(ensure-object '(function find-value-cell)))
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
;;; Find all external in an AST.

(defun find-externals (ast)
  (labels ((traverse (ast in-function-p)
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
				    (typep ast 'sicl-ast:function-ast))))))
    (traverse ast nil)))

