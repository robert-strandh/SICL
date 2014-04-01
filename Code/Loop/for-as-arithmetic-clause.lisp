;;;; Copyright (c) 2008, 2009, 2010, 2011, 2012, 2013, 2014
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

(in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ARITHMETIC.

(defclass for-as-arithmetic-clause (for-as-clause) ())

(defclass for-as-arithmetic (for-as-subclause) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for simple variable.

(define-parser simple-var-parser
  (singleton #'identity
	     (lambda (x) (and (symbolp x) (not (constantp x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers for individual keywords.

(defun project-form (keyword form)
  (declare (ignore keyword))
  form)

(define-parser anything-parser
  (singleton #'identity (constantly t)))

(define-parser from-parser
  (consecutive 'project-form (keyword-parser 'from) 'anything-parser))

(define-parser upfrom-parser
  (consecutive 'project-form (keyword-parser 'upfrom) 'anything-parser))

(define-parser downfrom-parser
  (consecutive 'project-form (keyword-parser 'downfrom) 'anything-parser))

(define-parser to-parser
  (consecutive (lambda (keyword form)
		 (declare (ignore keyword))
		 (cons '= form))
	       (keyword-parser 'to)
	       'anything-parser))

(define-parser upto-parser
  (consecutive (lambda (keyword form)
		 (declare (ignore keyword))
		 (cons '= form))
	       (keyword-parser 'upto)
	       'anything-parser))

(define-parser below-parser
  (consecutive (lambda (keyword form)
		 (declare (ignore keyword))
		 (cons '/= form))
	       (keyword-parser 'below)
	       'anything-parser))

(define-parser downto-parser
  (consecutive (lambda (keyword form)
		 (declare (ignore keyword))
		 (cons '= form))
	       (keyword-parser 'downto)
	       'anything-parser))

(define-parser above-parser
  (consecutive (lambda (keyword form)
		 (declare (ignore keyword))
		 (cons '/= form))
	       (keyword-parser 'above)
	       'anything-parser))

(define-parser by-parser
  (consecutive 'project-form (keyword-parser 'by) 'anything-parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers for arithmetic up.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers where FROM/UPFROM TO/UPTO/BELOW and BY are all present.
;;; Since they can appear in any order, there are 6 different
;;; variations.

;;; Order is FROM TO BY.
(define-parser arithmetic-up-1-parser
  (consecutive (lambda (var type-spec from to by)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,from)
				 (,to-var ,(cdr to))
				 (,by-var ,by))
		     :termination
		     `(when (,(if (eq (car to) '/=) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)
	       (alternative 'to-parser 'upto-parser 'below-parser)
	       'by-parser))

;;; Order is FROM BY TO.
(define-parser arithmetic-up-2-parser
  (consecutive (lambda (var type-spec from by to)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,from)
				 (,by-var ,by)
				 (,to-var ,(cdr to)))
		     :termination
		     `(when (,(if (eq (car to) '/=) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)
	       'by-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)))

;;; Order is TO FROM BY.
(define-parser arithmetic-up-3-parser
  (consecutive (lambda (var type-spec to from by)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,to-var ,(cdr to))
				 (,var ,from)
				 (,by-var ,by))
		     :termination
		     `(when (,(if (eq (car to) '/=) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)
	       (alternative 'from-parser 'upfrom-parser)
	       'by-parser))

;;; Order is TO BY FROM.
(define-parser arithmetic-up-4-parser
  (consecutive (lambda (var type-spec to by from)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,to-var ,(cdr to))
				 (,by-var ,by)
				 (,var ,from))
		     :termination
		     `(when (,(if (eq (car to) '/=) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)
	       'by-parser
	       (alternative 'from-parser 'upfrom-parser)))

;;; Order is BY FROM TO.
(define-parser arithmetic-up-5-parser
  (consecutive (lambda (var type-spec by from to)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,by-var ,by)
				 (,var ,from)
				 (,to-var ,(cdr to)))
		     :termination
		     `(when (,(if (eq (car to) '/=) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       'by-parser
	       (alternative 'from-parser 'upfrom-parser)
	       (alternative 'to-parser 'upto-parser 'below-parser)))

;;; Order is BY TO FROM.
(define-parser arithmetic-up-6-parser
  (consecutive (lambda (var type-spec by to from)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,by-var ,by)
				 (,to-var ,(cdr to))
				 (,var ,from))
		     :termination
		     `(when (,(if (eq (car to) '/=) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       'by-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)
	       (alternative 'from-parser 'upfrom-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers where only FROM/UPFROM and TO/UPTO/BELOW appear (BY is
;;; omitted).  Since they can appear in any order, there are 2
;;; different variations.

;;; Order is FROM TO.
(define-parser arithmetic-up-7-parser
  (consecutive (lambda (var type-spec from to)
		 (let ((to-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,from)
				 (,to-var ,(cdr to)))
		     :termination
		     `(when (,(if (eq (car to) '/=) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)
	       (alternative 'to-parser 'upto-parser 'below-parser)))

;;; Order is TO FROM.
(define-parser arithmetic-up-8-parser
  (consecutive (lambda (var type-spec to from)
		 (let ((to-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,to-var ,(cdr to))
				 (,var ,from))
		     :termination
		     `(when (,(if (eq (car to) '/=) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)
	       (alternative 'from-parser 'upfrom-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers where only FROM/UPFROM and BY appear (TO/UPTO/BELOW is
;;; omitted).  Since they can appear in any order, there are 2
;;; different variations.

;;; Order is FROM BY.
(define-parser arithmetic-up-9-parser
  (consecutive (lambda (var type-spec from by)
		 (let ((by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,from)
				 (,by-var ,by))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)
	       'by-parser))

;;; Order is BY FROM.
(define-parser arithmetic-up-10-parser
  (consecutive (lambda (var type-spec by from)
		 (let ((by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,by-var ,by)
				 (,var ,from))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       'by-parser
	       (alternative 'from-parser 'upfrom-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers where only TO/UPTO/BELOW and BY appear (FROM/UPFROM is
;;; omitted).  Since they can appear in any order, there are 2
;;; different variations.

;;; Order is TO BY.
(define-parser arithmetic-up-11-parser
  (consecutive (lambda (var type-spec to by)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var 0)
				 (,to-var ,(cdr to))
				 (,by-var ,by))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)
	       'by-parser))

;;; Order is BY TO.
(define-parser arithmetic-up-12-parser
  (consecutive (lambda (var type-spec by to)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var 0)
				 (,by-var ,by)
				 (,to-var ,(cdr to)))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       'by-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser where only FROM/UPFROM appears (TO/UPTO/BELOW and BY are
;;; omitted).

(define-parser arithmetic-up-13-parser
  (consecutive (lambda (var type-spec from)
		 (make-instance 'for-as-arithmetic
		   :bindings `((,var ,from))
		   :termination nil
		   :step `(incf ,var)))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser where only TO/UPTO/BELOW appears (FROM/UPFROM and BY are
;;; omitted).

(define-parser arithmetic-up-13-parser
  (consecutive (lambda (var type-spec to)
		 (let ((to-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,0)
				 (,to-var ,(cdr to)))
		     :termination
		     `(when (,(if (eq (car to) '/=) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser where only BY appears (FROM/UPFROM and TO/UPTO/BELOW are
;;; omitted).

(define-parser arithmetic-up-13-parser
  (consecutive (lambda (var type-spec by)
		 (let ((by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,0)
				 (,by-var ,by))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       'by-parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers for arithmetic down.
;;;
;;; There is no default start value for decremental stepping, so
;;; either FROM or DOWNFROM must always be supplied.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers where FROM/DOWNFROM TO/DOWNTO/ABOVE and BY are all present.
;;;
;;; The combination FROM - TO is not allowed. 

;;; FROM/DOWNFROM - DOWNTO/ABOVE - BY
(define-parser arithmetic-down-1-parser
  (consecutive (lambda (var type-spec from to by)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings '((,var ,from)
				 (,to-var (cdr to))
				 (,by-var ,by))
		     :termination
		     `(when (,(if (eq (car to) '/=) '<= '<) ,var ,to-var)
			(go end))
		     :step `(decf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'from-parser 'downfrom-parser)
	       (alternative 'downto-parser 'above-parser)
	       'by-parser))

;;; FROM/DOWNFROM - BY - DOWNTO/ABOVE 
(define-parser arithmetic-down-2-parser
  (consecutive (lambda (var type-spec from by to)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings '((,var ,from)
				 (,by-var ,by)
				 (,to-var (cdr to)))
		     :termination
		     `(when (,(if (eq (car to) '/=) '<= '<) ,var ,to-var)
			(go end))
		     :step `(decf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'from-parser 'downfrom-parser)
	       'by-parser
	       (alternative 'downto-parser 'above-parser)))

;;; DOWNTO/ABOVE - FROM/DOWNFROM - BY
(define-parser arithmetic-down-3-parser
  (consecutive (lambda (var type-spec to from by)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings '((,to-var (cdr to))
				 (,var ,from)
				 (,by-var ,by))
		     :termination
		     `(when (,(if (eq (car to) '/=) '<= '<) ,var ,to-var)
			(go end))
		     :step `(decf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'downto-parser 'above-parser)
	       (alternative 'from-parser 'downfrom-parser)
	       'by-parser))

;;; DOWNTO/ABOVE - BY - FROM/DOWNFROM
(define-parser arithmetic-down-4-parser
  (consecutive (lambda (var type-spec to by from)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings '((,to-var (cdr to))
				 (,by-var ,by)
				 (,var ,from))
		     :termination
		     `(when (,(if (eq (car to) '/=) '<= '<) ,var ,to-var)
			(go end))
		     :step `(decf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'downto-parser 'above-parser)
	       'by-parser
	       (alternative 'from-parser 'downfrom-parser)))

;;; BY- FROM/DOWNFROM - DOWNTO/ABOVE
(define-parser arithmetic-down-5-parser
  (consecutive (lambda (var type-spec by from to)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings '((,by-var ,by)
				 (,var ,from)
				 (,to-var (cdr to)))
		     :termination
		     `(when (,(if (eq (car to) '/=) '<= '<) ,var ,to-var)
			(go end))
		     :step `(decf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       'by-parser
	       (alternative 'from-parser 'downfrom-parser)
	       (alternative 'downto-parser 'above-parser)))

;;; BY- DOWNTO/ABOVE - FROM/DOWNFROM 
(define-parser arithmetic-down-6-parser
  (consecutive (lambda (var type-spec by to from)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings '((,by-var ,by)
				 (,to-var (cdr to))
				 (,var ,from))
		     :termination
		     `(when (,(if (eq (car to) '/=) '<= '<) ,var ,to-var)
			(go end))
		     :step `(decf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       'by-parser
	       (alternative 'downto-parser 'above-parser)
	       (alternative 'from-parser 'downfrom-parser)))

;;; DOWNFROM - TO/DOWNTO/ABOVE - BY
(define-parser arithmetic-down-7-parser
  (consecutive (lambda (var type-spec from to by)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings '((,var ,from)
				 (,to-var (cdr to))
				 (,by-var ,by))
		     :termination
		     `(when (,(if (eq (car to) '/=) '<= '<) ,var ,to-var)
			(go end))
		     :step `(decf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       'downfrom-parser
	       (alternative 'to-parser 'downto-parser 'above-parser)
	       'by-parser))

;;; DOWNFROM - BY - TO/DOWNTO/ABOVE 
(define-parser arithmetic-down-8-parser
  (consecutive (lambda (var type-spec from by to)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings '((,var ,from)
				 (,by-var ,by)
				 (,to-var (cdr to)))
		     :termination
		     `(when (,(if (eq (car to) '/=) '<= '<) ,var ,to-var)
			(go end))
		     :step `(decf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       'downfrom-parser
	       'by-parser
	       (alternative 'to-parser 'downto-parser 'above-parser)))

;;; TO/DOWNTO/ABOVE - DOWNFROM - BY
(define-parser arithmetic-down-9-parser
  (consecutive (lambda (var type-spec to from by)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings '((,to-var (cdr to))
				 (,var ,from)
				 (,by-var ,by))
		     :termination
		     `(when (,(if (eq (car to) '/=) '<= '<) ,var ,to-var)
			(go end))
		     :step `(decf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'to-parser 'downto-parser 'above-parser)
	       'downfrom-parser
	       'by-parser))
