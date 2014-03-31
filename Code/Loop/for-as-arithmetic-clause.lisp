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

(define-parser arithmetic-up-1-parser
  (consecutive (lambda (var type-spec from form1 to form2 by form3)
		 (declare (ignore from to by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,form1)
				 (,to-var ,form2)
				 (,by-var ,form3))
		     :termination `(when (>= ,var ,to-var) (go end))
		     :step `(incf ,var ,by-var))))
	       (singleton #'identity
			  (lambda (x) (and (symbolp x) (not (constantp x)))))
	       'type-spec-parser
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto))
	       (singleton #'identity (constantly t))
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))))

(define-parser arithmetic-up-2-parser
  (consecutive (lambda (var type-spec from form1 by form2 to form3)
		 (declare (ignore from to by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,form1)
				 (,by-var ,form2)
				 (,to-var ,form3))
		     :termination `(when (>= ,var ,to-var) (go end))
		     :step `(incf ,var ,by-var))))
	       (singleton #'identity
			  (lambda (x) (and (symbolp x) (not (constantp x)))))
	       'type-spec-parser
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto))
	       (singleton #'identity (constantly t))))

(define-parser arithmetic-up-3-parser
  (consecutive (lambda (var type-spec to form1 from form2 by form3)
		 (declare (ignore from to by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,to-var ,form1)
				 (,var ,form2)
				 (,by-var ,form3))
		     :termination `(when (>= ,var ,to-var) (go end))
		     :step `(incf ,var ,by-var))))
	       (singleton #'identity
			  (lambda (x) (and (symbolp x) (not (constantp x)))))
	       'type-spec-parser
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto))
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))))

(define-parser arithmetic-up-4-parser
  (consecutive (lambda (var type-spec to form1 by form2 from form3)
		 (declare (ignore from to by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,to-var ,form1)
				 (,by-var ,form2)
				 (,var ,form3))
		     :termination `(when (>= ,var ,to-var) (go end))
		     :step `(incf ,var ,by-var))))
	       (singleton #'identity
			  (lambda (x) (and (symbolp x) (not (constantp x)))))
	       'type-spec-parser
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto))
	       (singleton #'identity (constantly t))
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))))

(define-parser arithmetic-up-5-parser
  (consecutive (lambda (var type-spec by form1 from form2 to form3)
		 (declare (ignore from to by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,by-var ,form1)
				 (,var ,form2)
				 (,to-var ,form3))
		     :termination `(when (>= ,var ,to-var) (go end))
		     :step `(incf ,var ,by-var))))
	       (singleton #'identity
			  (lambda (x) (and (symbolp x) (not (constantp x)))))
	       'type-spec-parser
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto))
	       (singleton #'identity (constantly t))))

(define-parser arithmetic-up-6-parser
  (consecutive (lambda (var type-spec by form1 to form2 from form3)
		 (declare (ignore from to by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,by-var ,form1)
				 (,to-var ,form2)
				 (,var ,form3))
		     :termination `(when (>= ,var ,to-var) (go end))
		     :step `(incf ,var ,by-var))))
	       (singleton #'identity
			  (lambda (x) (and (symbolp x) (not (constantp x)))))
	       'type-spec-parser
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto))
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))))

(define-parser arithmetic-up-7-parser
  (consecutive (lambda (var type-spec from form1 to form2)
		 (declare (ignore from to))
		 (let ((to-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,form1)
				 (,to-var ,form2))
		     :termination `(when (>= ,var ,to-var) (go end))
		     :step `(incf ,var))))
	       (singleton #'identity
			  (lambda (x) (and (symbolp x) (not (constantp x)))))
	       'type-spec-parser
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto))
	       (singleton #'identity (constantly t))))

(define-parser arithmetic-up-8-parser
  (consecutive (lambda (var type-spec to form1 from form2)
		 (declare (ignore from to))
		 (let ((to-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,to-var ,form1)
				 (,var ,form2))
		     :termination `(when (>= ,var ,to-var) (go end))
		     :step `(incf ,var))))
	       (singleton #'identity
			  (lambda (x) (and (symbolp x) (not (constantp x)))))
	       'type-spec-parser
	       (alternative (keyword-parser 'to)
			    (keyword-parser 'upto))
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))))

(define-parser arithmetic-up-9-parser
  (consecutive (lambda (var type-spec from form1 by form2)
		 (declare (ignore from to by))
		 (let ((by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var ,form1)
				 (,by-var ,form3))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       (singleton #'identity
			  (lambda (x) (and (symbolp x) (not (constantp x)))))
	       'type-spec-parser
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))))

(define-parser arithmetic-up-9-parser
  (consecutive (lambda (var type-spec by form1 from form2)
		 (declare (ignore from to by))
		 (let ((by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,by-var ,form1)
				 (,var ,form3))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       (singleton #'identity
			  (lambda (x) (and (symbolp x) (not (constantp x)))))
	       'type-spec-parser
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))))

(define-parser arithmetic-up-9-parser
  (consecutive (lambda (var type-spec to form1 by form2)
		 (declare (ignore from to by))
		 (let ((to-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-arithmetic
		     :bindings `((,var 0)
				 (,to-var ,form1)
				 (,by-var ,form3))
		     :termination nil
		     :step `(incf ,var ,by-var))))
	       (singleton #'identity
			  (lambda (x) (and (symbolp x) (not (constantp x)))))
	       'type-spec-parser
	       (alternative (keyword-parser 'from)
			    (keyword-parser 'upfrom))
	       (singleton #'identity (constantly t))
	       (keyword-parser 'by)
	       (singleton #'identity (constantly t))))

