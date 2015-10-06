;;;; Copyright (c) 2014 - 2015
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

(cl:in-package #:sicl-loop)

;;; The purpose of this generic function is to generate a list of all
;;; bound variables in a clause.  The same variable occurs as many
;;; times in the list as the number of times it is bound in the
;;; clause.
(defgeneric bound-variables (clause))

;;; The purpose of this generic function is to generate a list of all
;;; the accumulation variables in a clause.  Each element of the list
;;; is itself a list of three elements.  The first element is the name
;;; of a variable used in an INTO clause, or NIL if the clause has no
;;; INTO.  The second element determines the kind of accumulation, and
;;; can be one of the symbols LIST, COUNT/SUM, or MAX/MIN.  The third
;;; element is a type specifier which can be T.
(defgeneric accumulation-variables (clause))

(defgeneric declarations (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric initial-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric final-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric bindings (clause)
  (:method (clause)
    (append (initial-bindings clause) (final-bindings clause))))

(defgeneric prologue-form (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

(defgeneric termination-form (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

(defgeneric body-form (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

(defgeneric step-form (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric epilogue (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

;;; This variable is bound by the code generator for
;;; CONDITIONAL-CLAUSE before calling the code generators for the
;;; clauses in its THEN and ELSE branches.
(defvar *it-var*)

(defvar *accumulation-variable*)

(defvar *list-tail-accumulation-variable*)

(defvar *tail-variables*)

(defun tail-variable (head-variable)
  (let ((result (gethash head-variable *tail-variables*)))
    (when (null result)
      (setf result (gensym))
      (setf (gethash head-variable *tail-variables*) result))
    result))

(defun accumulation-bindings (clauses)
  (let* ((descriptors
	   (reduce #'append
		   (mapcar #'accumulation-variables clauses)))
	 (equal-fun (lambda (d1 d2)
		      (and (eq (first d1) (first d2))
			   (eq (second d1) (second d2)))))
	 (unique (remove-duplicates descriptors :test equal-fun)))
    (loop for (name category type) in unique
	  for initial-value = (cond  ((eq category 'count/sum)
				      (coerce 0 type))
				     ((eq category 'always/never)
				      t)
				     (t
				      nil))
	  collect (if (null name)
		      `(,*accumulation-variable* ,initial-value)
		      `(,name ,initial-value))
	  when (eq category 'list)
	    collect (if (null name)
		      `(,*list-tail-accumulation-variable* nil)
		      `(,(tail-variable name) nil)))))

(defvar *loop-name*)

(defun prologue-body-epilogue (clauses end-tag)
  (let ((start-tag (gensym)))
    `(tagbody
	(progn ,@(mapcar (lambda (clause)
			   (prologue-form clause end-tag))
			 clauses))
	,start-tag
	(progn ,@(mapcar (lambda (clause)
			   (body-form clause end-tag))
			 clauses))
	(progn ,@(mapcar (lambda (clause)
			   (termination-form clause end-tag))
			 clauses))
	(progn ,@(mapcar #'step-form clauses))
	(go ,start-tag)
	,end-tag
	(progn ,@(mapcar #'epilogue clauses)
	       (return-from ,*loop-name*
		 ,*accumulation-variable*)))))

(defgeneric wrap-clause (clause inner-form))

(defgeneric wrap-subclause (subclause inner-form))

(defmethod wrap-subclause (subclause inner-form)
  `(let ,(final-bindings subclause)
     ,inner-form))

(defmethod wrap-clause (clause inner-form)
  `(let* ,(bindings clause)
     ,inner-form))

(defmethod wrap-clause ((clause subclauses-mixin) inner-form)
  (let ((result inner-form))
    (mapc (lambda (subclause)
	    (setf result (wrap-subclause subclause result)))
	  (reverse (subclauses clause)))
    `(let ,(initial-bindings clause)
       ,result)))

(defun do-clauses (all-clauses end-tag)
  (let ((result (prologue-body-epilogue all-clauses end-tag)))
    (mapc (lambda (clause)
	    (setf result (wrap-clause clause result)))
	  (reverse all-clauses))
    result))

(defun expand-clauses (all-clauses end-tag)
  (let ((acc (accumulation-bindings all-clauses)))
    `(let (,@(if (member *accumulation-variable* acc :key #'car)
		 '()
		 `((,*accumulation-variable* nil)))
	   ,@acc)
       ,(do-clauses all-clauses end-tag))))

(defun expand-body (loop-body end-tag)
  (if (every #'consp loop-body)
      (let ((tag (gensym)))
	`(block nil
	   (tagbody
	      ,tag
	      ,@loop-body
	      (go ,tag))))
      (let ((clauses (parse-loop-body loop-body)))
	(analyze-clauses clauses)
	(let* ((name (if (typep (car clauses) 'name-clause)
			 (name (car clauses))
			 nil))
	       (*loop-name* name)
	       (*accumulation-variable* (gensym))
	       (*list-tail-accumulation-variable* (gensym))
	       (*tail-variables* (make-hash-table :test #'eq)))
	  `(block ,name
	     ,(expand-clauses clauses end-tag))))))
