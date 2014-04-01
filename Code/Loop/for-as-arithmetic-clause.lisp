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
;;; Persers where FROM/UPFROM TO/UPTO and BY are all present.
;;; Since they can appear in any order, there are 6 different
;;; variations. 

;;; Order is FROM/UPFROM TO/UPTO BY.
(define-parser arithmetic-up-1-parser
  (consecutive (lambda (var type-spec from to form2 by)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,from)
				 (,to-var ,form2)
				 (,by-var ,by))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))
	       'by-parser))

;;; Order is FROM/UPFROM BY TO/UPTO.
(define-parser arithmetic-up-2-parser
  (consecutive (lambda (var type-spec from by to form3)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,from)
				 (,by-var ,by)
				 (,to-var ,form3))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)
	       'by-parser
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))))

;;; Order is TO/UPTO FROM/UPFROM BY.
(define-parser arithmetic-up-3-parser
  (consecutive (lambda (var type-spec to form1 from by)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,to-var ,form1)
				 (,var ,from)
				 (,by-var ,by))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))
	       (alternative 'from-parser 'upfrom-parser)
	       'by-parser))

;;; Order is TO/UPTO BY FROM/UPFROM.
(define-parser arithmetic-up-4-parser
  (consecutive (lambda (var type-spec to form1 by from)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,to-var ,form1)
				 (,by-var ,by)
				 (,var ,from))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))
	       'by-parser
	       (alternative 'from-parser 'upfrom-parser)))

;;; Order is BY FROM/UPFROM TO/UPTO.
(define-parser arithmetic-up-5-parser
  (consecutive (lambda (var type-spec by from to form3)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,by-var ,by)
				 (,var ,from)
				 (,to-var ,form3))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       'by-parser
	       (alternative 'from-parser 'upfrom-parser)
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))))

;;; Order is BY TO/UPTO FROM/UPFROM.
(define-parser arithmetic-up-6-parser
  (consecutive (lambda (var type-spec by to form2 from)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,by-var ,by)
				 (,to-var ,form2)
				 (,var ,from))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       'by-parser
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))
	       (alternative 'from-parser 'upfrom-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Persers where only FROM/UPFROM and TO/UPTO appear (BY is omitted).
;;; Since they can appear in any order, there are 2 different
;;; variations.

;;; Order is FROM/UPFROM TO/UPTO.
(define-parser arithmetic-up-7-parser
  (consecutive (lambda (var type-spec from to form2)
		 (let ((to-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,from)
				 (,to-var ,form2))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))))

;;; Order is TO/UPTO FROM/UPFROM.
(define-parser arithmetic-up-8-parser
  (consecutive (lambda (var type-spec to form1 from)
		 (let ((to-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,to-var ,form1)
				 (,var ,from))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))
	       (alternative 'from-parser 'upfrom-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Persers where only FROM/UPFROM and BY appear (TO/UPTO is omitted).
;;; Since they can appear in any order, there are 2 different
;;; variations.

;;; Order is FROM/UPFROM BY.
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

;;; Order is BY FROM/UPFROM.
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
;;; Persers where only TO/UPTO and BY appear (FROM/UPFROM is omitted).
;;; Since they can appear in any order, there are 2 different
;;; variations.

;;; Order is TO/UPTO BY.
(define-parser arithmetic-up-11-parser
  (consecutive (lambda (var type-spec to form1 by)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var 0)
				 (,to-var ,form1)
				 (,by-var ,by))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))
	       'by-parser))

;;; Order is BY TO/UPTO.
(define-parser arithmetic-up-12-parser
  (consecutive (lambda (var type-spec by to form2)
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var 0)
				 (,by-var ,by)
				 (,to-var ,form3))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       'by-parser
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Perser where only FROM/UPFROM appears (TO/UPTO and BY are
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
;;; Perser where only TO/UPTO appears (FROM/UPFROM and BY are
;;; omitted).

(define-parser arithmetic-up-13-parser
  (consecutive (lambda (var type-spec to form1)
		 (let ((to-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,0)
				 (,to-var,form1))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Perser where only BY appears (FROM/UPFROM and TO/UPTO are
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

