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

(in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ARITHMETIC.

(defclass for-as-arithmetic (for-as-subclause var-and-type-spec-mixin)
  (;; The form that was given after one of the LOOP keywords FROM,
   ;; UPFROM, or DOWNFROM, or 0 if none of these LOOP keywords was
   ;; given.
   (%start-form :initform 0 :initarg :start-form :reader start-form)
   (%start-var :initform (gensym) :reader start-var)
   ;; The form that was after one of the LOOP keywords TO, UPTO,
   ;; DOWNTO, BELOW, or ABOVE, or NIL if none of these LOOP keywords
   ;; was given.
   (%end-form :initform nil :initarg :end-form :reader end-form)
   (%end-var :initform (gensym) :reader end-var)
   ;; The form that was after the LOOP keyword BY, or 0 if this
   ;; keyword was not given.
   (%by-form :initform 1 :initarg :by-form :reader by-form)
   (%by-var :initform (gensym) :reader by-var)
   ;; If termination is TO, UPTO, or DOWNTO, then this slot contains
   ;; the symbol <=.  If termination is ABOVE or BELOW, then this slot
   ;; contains the symbol <.
   (%termination-test :initform '<=
		      :initarg :termination-test
		      :reader termination-test)))

(defclass for-as-arithmetic-up (for-as-arithmetic) ()
  (:default-initargs :termination-test '<=))

(defclass for-as-arithmetic-down (for-as-arithmetic) ()
  (:default-initargs :termination-test '>=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a TYPE-SPEC determine a value used for variable
;;; initialization and a type to use in a declaration, and return them
;;; as two values.  The type returned may be different from the
;;; TYPE-SPEC argument because we may not be able to determine a
;;; initialization value that would conform to the TYPE-SPEC, and in
;;; that case, we must modify the type so that it covers the
;;; initialization value that we give.
;;;
;;; Perhaps this code should be moved to the code utilities module.

(defun arithmetic-value-and-type (type-spec)
  (cond ((eq type-spec 'fixnum)
	 (values 0 type-spec))
	((eq type-spec 'float)
	 (values 0.0 type-spec))
	((eq type-spec 'short-float)
	 (values 0s0 type-spec))
	((eq type-spec 'single-float)
	 (values 0f0 type-spec))
	((eq type-spec 'double-float)
	 (values 0d0 type-spec))
	((eq type-spec 'long-float)
	 (values 0l0 type-spec))
	((and (consp type-spec)
	      (eq (car type-spec) 'integer)
	      (consp (cdr type-spec))
	      (integerp (cadr type-spec)))
	 (values (cadr type-spec) type-spec))
	;; We could add some more here, for instance intervals
	;; of floats.
	(t
	 (values 0 't))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser for-as-arithmetic-parser
  (alternative 'arithmetic-up-parser
	       'arithmetic-down-parser))

(add-for-as-subclause-parser 'for-as-arithmetic-parser)

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
		 (cons '<= form))
	       (keyword-parser 'to)
	       'anything-parser))

(define-parser upto-parser
  (consecutive (lambda (keyword form)
		 (declare (ignore keyword))
		 (cons '<= form))
	       (keyword-parser 'upto)
	       'anything-parser))

(define-parser below-parser
  (consecutive (lambda (keyword form)
		 (declare (ignore keyword))
		 (cons '< form))
	       (keyword-parser 'below)
	       'anything-parser))

(define-parser downto-parser
  (consecutive (lambda (keyword form)
		 (declare (ignore keyword))
		 (cons '<= form))
	       (keyword-parser 'downto)
	       'anything-parser))

(define-parser above-parser
  (consecutive (lambda (keyword form)
		 (declare (ignore keyword))
		 (cons '< form))
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
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)
	       (alternative 'to-parser 'upto-parser 'below-parser)
	       'by-parser))

;;; Order is FROM BY TO.
(define-parser arithmetic-up-2-parser
  (consecutive (lambda (var type-spec from by to)
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)
	       'by-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)))

;;; Order is TO FROM BY.
(define-parser arithmetic-up-3-parser
  (consecutive (lambda (var type-spec to from by)
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)
	       (alternative 'from-parser 'upfrom-parser)
	       'by-parser))

;;; Order is TO BY FROM.
(define-parser arithmetic-up-4-parser
  (consecutive (lambda (var type-spec to by from)
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)
	       'by-parser
	       (alternative 'from-parser 'upfrom-parser)))

;;; Order is BY FROM TO.
(define-parser arithmetic-up-5-parser
  (consecutive (lambda (var type-spec by from to)
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       'by-parser
	       (alternative 'from-parser 'upfrom-parser)
	       (alternative 'to-parser 'upto-parser 'below-parser)))

;;; Order is BY TO FROM.
(define-parser arithmetic-up-6-parser
  (consecutive (lambda (var type-spec by to from)
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
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
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)
	       (alternative 'to-parser 'upto-parser 'below-parser)))

;;; Order is TO FROM.
(define-parser arithmetic-up-8-parser
  (consecutive (lambda (var type-spec to from)
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
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
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :by-form by))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)
	       'by-parser))

;;; Order is BY FROM.
(define-parser arithmetic-up-10-parser
  (consecutive (lambda (var type-spec by from)
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :by-form by))
	       'simple-var-parser
	       'optional-type-spec-parser
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
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)
	       'by-parser))

;;; Order is BY TO.
(define-parser arithmetic-up-12-parser
  (consecutive (lambda (var type-spec by to)
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       'by-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser where only FROM/UPFROM appears (TO/UPTO/BELOW and BY are
;;; omitted).

(define-parser arithmetic-up-13-parser
  (consecutive (lambda (var type-spec from)
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :start-form from))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'from-parser 'upfrom-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser where only TO/UPTO/BELOW appears (FROM/UPFROM and BY are
;;; omitted).

(define-parser arithmetic-up-14-parser
  (consecutive (lambda (var type-spec to)
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :end-form (second to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'to-parser 'upto-parser 'below-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser where only BY appears (FROM/UPFROM and TO/UPTO/BELOW are
;;; omitted).

(define-parser arithmetic-up-15-parser
  (consecutive (lambda (var type-spec by)
		 (make-instance 'for-as-arithmetic-up
		   :var-spec var
		   :type-spec type-spec
		   :by-form by))
	       'simple-var-parser
	       'optional-type-spec-parser
	       'by-parser))

(define-parser arithmetic-up-parser
  (alternative 'arithmetic-up-1-parser
	       'arithmetic-up-2-parser
	       'arithmetic-up-3-parser
	       'arithmetic-up-4-parser
	       'arithmetic-up-5-parser
	       'arithmetic-up-6-parser
	       'arithmetic-up-7-parser
	       'arithmetic-up-8-parser
	       'arithmetic-up-9-parser
	       'arithmetic-up-10-parser
	       'arithmetic-up-11-parser
	       'arithmetic-up-12-parser
	       'arithmetic-up-13-parser))

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
		 (make-instance 'for-as-arithmetic-down
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'from-parser 'downfrom-parser)
	       (alternative 'downto-parser 'above-parser)
	       'by-parser))

;;; FROM/DOWNFROM - BY - DOWNTO/ABOVE 
(define-parser arithmetic-down-2-parser
  (consecutive (lambda (var type-spec from by to)
		 (make-instance 'for-as-arithmetic-down
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'from-parser 'downfrom-parser)
	       'by-parser
	       (alternative 'downto-parser 'above-parser)))

;;; DOWNTO/ABOVE - FROM/DOWNFROM - BY
(define-parser arithmetic-down-3-parser
  (consecutive (lambda (var type-spec to from by)
		 (make-instance 'for-as-arithmetic-down
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'downto-parser 'above-parser)
	       (alternative 'from-parser 'downfrom-parser)
	       'by-parser))

;;; DOWNTO/ABOVE - BY - FROM/DOWNFROM
(define-parser arithmetic-down-4-parser
  (consecutive (lambda (var type-spec to by from)
		 (make-instance 'for-as-arithmetic-down
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'downto-parser 'above-parser)
	       'by-parser
	       (alternative 'from-parser 'downfrom-parser)))

;;; BY- FROM/DOWNFROM - DOWNTO/ABOVE
(define-parser arithmetic-down-5-parser
  (consecutive (lambda (var type-spec by from to)
		 (make-instance 'for-as-arithmetic-down
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       'by-parser
	       (alternative 'from-parser 'downfrom-parser)
	       (alternative 'downto-parser 'above-parser)))

;;; BY- DOWNTO/ABOVE - FROM/DOWNFROM 
(define-parser arithmetic-down-6-parser
  (consecutive (lambda (var type-spec by to from)
		 (make-instance 'for-as-arithmetic-down
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       'by-parser
	       (alternative 'downto-parser 'above-parser)
	       (alternative 'from-parser 'downfrom-parser)))

;;; DOWNFROM - TO/DOWNTO/ABOVE - BY
(define-parser arithmetic-down-7-parser
  (consecutive (lambda (var type-spec from to by)
		 (make-instance 'for-as-arithmetic-down
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       'downfrom-parser
	       (alternative 'to-parser 'downto-parser 'above-parser)
	       'by-parser))

;;; DOWNFROM - BY - TO/DOWNTO/ABOVE 
(define-parser arithmetic-down-8-parser
  (consecutive (lambda (var type-spec from by to)
		 (make-instance 'for-as-arithmetic-down
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       'downfrom-parser
	       'by-parser
	       (alternative 'to-parser 'downto-parser 'above-parser)))

;;; TO/DOWNTO/ABOVE - DOWNFROM - BY
(define-parser arithmetic-down-9-parser
  (consecutive (lambda (var type-spec to from by)
		 (make-instance 'for-as-arithmetic-down
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'to-parser 'downto-parser 'above-parser)
	       'downfrom-parser
	       'by-parser))

;;; TO/DOWNTO/ABOVE - BY - DOWNFROM
(define-parser arithmetic-down-10-parser
  (consecutive (lambda (var type-spec to by from)
		 (make-instance 'for-as-arithmetic-down
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       (alternative 'to-parser 'downto-parser 'above-parser)
	       'by-parser
	       'downfrom-parser))

;;; BY- DOWNFROM - TO/DOWNTO/ABOVE
(define-parser arithmetic-down-11-parser
  (consecutive (lambda (var type-spec by from to)
		 (make-instance 'for-as-arithmetic-down
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       'by-parser
	       'downfrom-parser
	       (alternative 'to-parser 'downto-parser 'above-parser)))

;;; BY- TO/DOWNTO/ABOVE - DOWNFROM 
(define-parser arithmetic-down-12-parser
  (consecutive (lambda (var type-spec by to from)
		 (make-instance 'for-as-arithmetic-down
		   :var-spec var
		   :type-spec type-spec
		   :start-form from
		   :end-form (second to)
		   :by-form by
		   :termination-test (first to)))
	       'simple-var-parser
	       'optional-type-spec-parser
	       'by-parser
	       (alternative 'to-parser 'downto-parser 'above-parser)
	       'downfrom-parser))

(define-parser arithmetic-down-parser
  (alternative 'arithmetic-down-1-parser
	       'arithmetic-down-2-parser
	       'arithmetic-down-3-parser
	       'arithmetic-down-4-parser
	       'arithmetic-down-5-parser
	       'arithmetic-down-6-parser
	       'arithmetic-down-7-parser
	       'arithmetic-down-8-parser
	       'arithmetic-down-9-parser
	       'arithmetic-down-10-parser
	       'arithmetic-down-11-parser
	       'arithmetic-down-12-parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause for-as-arithmetic))
  `((,(start-var clause) ,(start-form clause))
    ,@(if (null (end-form clause))
	  '()
	  `(,(end-var clause) ,(end-form clause)))
    (,(by-var clause) ,(by-form clause))))

(defmethod final-bindings ((clause for-as-arithmetic))
  `((,(var-spec clause) ,(start-var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod declarations ((clause for-as-arithmetic))
  `((cl:type ,(type-spec clause) ,(var-spec clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination.

(defmethod termination ((clause for-as-arithmetic-up))
  `(unless (,(termination-test clause)
	    ,(var-spec clause)
	    ,(form-value-var clause))
     (go end)))

(defmethod termination ((clause for-as-arithmetic-down))
  `(unless (,(termination-test clause)
	    ,(form-value-var clause)
	    ,(var-spec clause))
     (go end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination.

(defmethod step ((clause for-as-arithmetic-up))
  `(incf ,(var-spec clause) ,(by-var clause)))

(defmethod step ((clause for-as-arithmetic-down))
  `(decf ,(var-spec clause) ,(by-var clause)))
