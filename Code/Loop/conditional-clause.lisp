;;;; Copyright (c) 2014
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

(define-parser selectable-clause-parser
  (alternative 'do-clause-parser
	       'return-clause-parser
	       'collect-clause-parser
	       'append-clause-parser
	       'nconc-clause-parser
	       'count-clause-parser
	       'sum-clause-parser
	       'maximize-clause-parser
	       'minimize-clause-parser
	       'conditional-clause-parser))
	       
(define-parser and-selectable-clause-parser
  (consecutive (lambda (and selectable-clause)
		 (declare (ignore and))
		 selectable-clause)
	       (keyword-parser 'and)
	       'selectable-clause-parser))

(define-parser then-or-else-parser
  (consecutive #'cons
	       'selectable-clause-parser
	       (repeat* #'list
			'and-selectable-clause-parser)))

(defclass conditional-clause (clause)
  ((%condition :initarg :condition :reader condition)
   (%then-clauses :initarg :then-clauses :reader then-clauses)
   (%else-clauses :initarg :else-clauses :reader else-clauses)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser if-else-end-clause-parser
  (consecutive (lambda (if form then-clauses else else-clauses end)
		 (declare (ignore if else end))
		 (make-instance 'conditional-clause
		   :condition form
		   :then-clauses then-clauses
		   :else-clauses else-clauses))
	       (alternative (keyword-parser 'if)
			    (keyword-parser 'when))
	       (singleton #'identity (constantly t))
	       'then-or-else-parser
	       (keyword-parser 'else)
	       'then-or-else-parser
	       (keyword-parser 'end)))

(define-parser if-end-clause-parser
  (consecutive (lambda (if form then-clauses end)
		 (declare (ignore if end))
		 (make-instance 'conditional-clause
		   :condition form
		   :then-clauses then-clauses
		   :else-clauses nil))
	       (alternative (keyword-parser 'if)
			    (keyword-parser 'when))
	       (singleton #'identity (constantly t))
	       'then-or-else-parser
	       (keyword-parser 'end)))
	       
(define-parser if-else-clause-parser
  (consecutive (lambda (if form then-clauses else else-clauses)
		 (declare (ignore if else))
		 (make-instance 'conditional-clause
		   :condition form
		   :then-clauses then-clauses
		   :else-clauses else-clauses))
	       (alternative (keyword-parser 'if)
			    (keyword-parser 'when))
	       (singleton #'identity (constantly t))
	       'then-or-else-parser
	       (keyword-parser 'else)
	       'then-or-else-parser))

(define-parser if-clause-parser
  (consecutive (lambda (if form then-clauses)
		 (declare (ignore if))
		 (make-instance 'conditional-clause
		   :condition form
		   :then-clauses then-clauses
		   :else-clauses nil))
	       (alternative (keyword-parser 'if)
			    (keyword-parser 'when))
	       (singleton #'identity (constantly t))
	       'then-or-else-parser))

(define-parser if-when-parser
  (alternative 'if-else-end-clause-parser
	       'if-end-clause-parser
	       'if-else-clause-parser
	       'if-clause-parser))

(define-parser unless-else-end-clause-parser
  (consecutive (lambda (unless form else-clauses else then-clauses end)
		 (declare (ignore unless else end))
		 (make-instance 'conditional-clause
		   :condition form
		   :then-clauses then-clauses
		   :else-clauses else-clauses))
	       (keyword-parser 'unless)
	       (singleton #'identity (constantly t))
	       'then-or-else-parser
	       (keyword-parser 'else)
	       'then-or-else-parser
	       (keyword-parser 'end)))

(define-parser unless-end-clause-parser
  (consecutive (lambda (unless form else-clauses end)
		 (declare (ignore unless end))
		 (make-instance 'conditional-clause
		   :condition form
		   :then-clauses nil
		   :else-clauses else-clauses))
	       (keyword-parser 'unless)
	       (singleton #'identity (constantly t))
	       'then-or-else-parser
	       (keyword-parser 'end)))
	       
(define-parser unless-else-clause-parser
  (consecutive (lambda (unless form else-clauses else then-clauses)
		 (declare (ignore unless else))
		 (make-instance 'conditional-clause
		   :condition form
		   :then-clauses then-clauses
		   :else-clauses else-clauses))
	       (keyword-parser 'unless)
	       (singleton #'identity (constantly t))
	       'then-or-else-parser
	       (keyword-parser 'else)
	       'then-or-else-parser))

(define-parser unless-clause-parser
  (consecutive (lambda (unless form else-clauses)
		 (declare (ignore unless))
		 (make-instance 'conditional-clause
		   :condition form
		   :then-clauses nil
		   :else-clauses else-clauses))
	       (alternative (keyword-parser 'unless)
			    (keyword-parser 'when))
	       (singleton #'identity (constantly t))
	       'then-or-else-parser))

(define-parser unless-parser
  (alternative 'unless-else-end-clause-parser
	       'unless-end-clause-parser
	       'unless-else-clause-parser
	       'unless-clause-parser))

(define-parser if-when-unless-parser
  (alternative 'if-when-parser
	       'unless-parser))
