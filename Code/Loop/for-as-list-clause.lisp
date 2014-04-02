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
;;; Clause FOR-AS-IN-LIST.

(defclass for-as-in-list (for-as-subclause) ())

(define-parser for-as-in-list-parser-1
  (consecutive (lambda (var type-spec in list by)
		 ;; FIXME: handle the var and the type-spec
		 (declare (ignore in var type-spec))
		 (let ((list-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-in-list
		     :bindings `((,list-var ,list)
				 (,by-var ,by))
		     :termination `(when (endp ,list-var)
				     (go end))
		     :step `(setf ,list-var (funcall ,by-var ,list-var)))))
	       'd-var-spec-parser
	       'type-spec-parser
	       (keyword-parser 'in)
	       'by-parser))

(define-parser for-as-in-list-parser-2
  (consecutive (lambda (var type-spec in list)
		 ;; FIXME: handle the var and the the type-spec
		 (declare (ignore in var type-spec))
		 (let ((list-var (gensym)))
		   (make-instance 'for-as-in-list
		     :bindings `((,list-var ,list))
		     :termination `(when (endp ,list-var)
				     (go end))
		     :step `(pop ,list-var))))
	       'd-var-spec-parser
	       'type-spec-parser
	       (keyword-parser 'in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ON-LIST.

(defclass for-as-on-list (for-as-subclause) ())

(define-parser for-as-on-list-parser-1
  (consecutive (lambda (var type-spec on list by)
		 ;; FIXME: handle the var and the type-spec
		 (declare (ignore on var type-spec))
		 (let ((list-var (gensym))
		       (by-var (gensym)))
		   (make-instance 'for-as-on-list
		     :bindings `((,list-var ,list)
				 (,by-var ,by))
		     :termination `(when (endp ,list-var)
				     (go end))
		     :step `(setf ,list-var (funcall ,by-var ,list-var)))))
	       'd-var-spec-parser
	       'type-spec-parser
	       (keyword-parser 'on)
	       'by-parser))

(define-parser for-as-on-list-parser-2
  (consecutive (lambda (var type-spec on list)
		 ;; FIXME: handle the var and the the type-spec
		 (declare (ignore on var type-spec))
		 (let ((list-var (gensym)))
		   (make-instance 'for-as-on-list
		     :bindings `((,list-var ,list))
		     :termination `(when (endp ,list-var)
				     (go end))
		     :step `(pop ,list-var))))
	       'd-var-spec-parser
	       'type-spec-parser
	       (keyword-parser 'on)))
