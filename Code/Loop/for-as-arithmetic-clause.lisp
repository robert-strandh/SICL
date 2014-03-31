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
