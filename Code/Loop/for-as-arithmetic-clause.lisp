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
;;; Parsers for arithmetic up.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Persers where FROM/UPFROM TO/UPTO and BY are all present.
;;; Since they can appear in any order, there are 6 different
;;; variations. 

;;; Order is FROM/UPFROM TO/UPTO BY.
(define-parser arithmetic-up-1-parser
  (consecutive (lambda (var type-spec from form1 to form2 by form3)
		 (declare (ignore from by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,form1)
				 (,to-var ,form2)
				 (,by-var ,form3))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))))

;;; Order is FROM/UPFROM BY TO/UPTO.
(define-parser arithmetic-up-2-parser
  (consecutive (lambda (var type-spec from form1 by form2 to form3)
		 (declare (ignore from by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,form1)
				 (,by-var ,form2)
				 (,to-var ,form3))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))))

;;; Order is TO/UPTO FROM/UPFROM BY.
(define-parser arithmetic-up-3-parser
  (consecutive (lambda (var type-spec to form1 from form2 by form3)
		 (declare (ignore from by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,to-var ,form1)
				 (,var ,form2)
				 (,by-var ,form3))
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
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))))

;;; Order is TO/UPTO BY FROM/UPFROM.
(define-parser arithmetic-up-4-parser
  (consecutive (lambda (var type-spec to form1 by form2 from form3)
		 (declare (ignore from by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,to-var ,form1)
				 (,by-var ,form2)
				 (,var ,form3))
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
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))))

;;; Order is BY FROM/UPFROM TO/UPTO.
(define-parser arithmetic-up-5-parser
  (consecutive (lambda (var type-spec by form1 from form2 to form3)
		 (declare (ignore from by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,by-var ,form1)
				 (,var ,form2)
				 (,to-var ,form3))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))))

;;; Order is BY TO/UPTO FROM/UPFROM.
(define-parser arithmetic-up-6-parser
  (consecutive (lambda (var type-spec by form1 to form2 from form3)
		 (declare (ignore from by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,by-var ,form1)
				 (,to-var ,form2)
				 (,var ,form3))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Persers where only FROM/UPFROM and TO/UPTO appear (BY is omitted).
;;; Since they can appear in any order, there are 2 different
;;; variations.

;;; Order is FROM/UPFROM TO/UPTO.
(define-parser arithmetic-up-7-parser
  (consecutive (lambda (var type-spec from form1 to form2)
		 (declare (ignore from))
		 (let ((to-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,form1)
				 (,to-var ,form2))
		     :termination
		     `(when (,(if (eq to 'below) '>= '>) ,var ,to-var)
			(go end))
		     :step `(incf ,var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))))

;;; Order is TO/UPTO FROM/UPFROM.
(define-parser arithmetic-up-8-parser
  (consecutive (lambda (var type-spec to form1 from form2)
		 (declare (ignore from))
		 (let ((to-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,to-var ,form1)
				 (,var ,form2))
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
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Persers where only FROM/UPFROM and BY appear (TO/UPTO is omitted).
;;; Since they can appear in any order, there are 2 different
;;; variations.

;;; Order is FROM/UPFROM BY.
(define-parser arithmetic-up-9-parser
  (consecutive (lambda (var type-spec from form1 by form2)
		 (declare (ignore from by))
		 (let ((by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,form1)
				 (,by-var ,form2))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))))

;;; Order is BY FROM/UPFROM.
(define-parser arithmetic-up-10-parser
  (consecutive (lambda (var type-spec by form1 from form2)
		 (declare (ignore from by))
		 (let ((by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,by-var ,form1)
				 (,var ,form2))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Persers where only TO/UPTO and BY appear (FROM/UPFROM is omitted).
;;; Since they can appear in any order, there are 2 different
;;; variations.

;;; Order is TO/UPTO BY.
(define-parser arithmetic-up-11-parser
  (consecutive (lambda (var type-spec to form1 by form2)
		 (declare (ignore by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var 0)
				 (,to-var ,form1)
				 (,by-var ,form2))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))))

;;; Order is BY TO/UPTO.
(define-parser arithmetic-up-12-parser
  (consecutive (lambda (var type-spec by form1 to form2)
		 (declare (ignore by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var 0)
				 (,by-var ,form1)
				 (,to-var ,form3))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto)
			    (keyword-parser 'below))
	       (singleton #'identity (constantly t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Perser where only FROM/UPFROM appears (TO/UPTO and BY are
;;; omitted).

(define-parser arithmetic-up-13-parser
  (consecutive (lambda (var type-spec from form1)
		 (declare (ignore from))
		 (make-instance 'for-as-arithmetic
		   :bindings `((,var ,form1))
		   :termination nil
		   :step `(incf ,var)))
	       'simple-var-parser
	       'type-spec-parser
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))))

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
  (consecutive (lambda (var type-spec by form1)
		 (declare (ignore by))
		 (let ((by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,0)
				 (,by-var,form1))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       'simple-var-parser
	       'type-spec-parser
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))))

