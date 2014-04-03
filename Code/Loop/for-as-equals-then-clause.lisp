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
;;; Clause FOR-AS-EQUALS-THEN.

(defclass for-as-equals-then (for-as-subclause) ())

(define-parser then-parser
  (consecutive 'project-form (keyword-parser 'then) 'anything-parser))

(define-parser for-as-equals-then-parser-1
  (consecutive (lambda (var type-spec = form1 then)
		 ;; FIXME: handle var and type-spec
		 (declare (ignore = var type-spec))
		 (let ((iteration-var (gensym)))
		   (make-instance 'for-equals-then
		     :bindings `((,iteration-var ,form1))
		     :termination nil
		     :step `(setf ,iteration-var ,then))))
	       'd-var-spec-parser
	       'type-spec-parser
	       (keyword-parser '=)
	       'then-parser))

(define-parser for-as-equals-then-parser-2
  (consecutive (lambda (var type-spec = form1)
		 ;; FIXME: handle var and type-spec
		 (declare (ignore = var type-spec))
		 (let ((iteration-var (gensym)))
		   (make-instance 'for-equals-then
		     :bindings `((,iteration-var ,form1))
		     :termination nil
		     :step `(setf ,iteration-var ,form1))))
	       'd-var-spec-parser
	       'type-spec-parser
	       (keyword-parser '=)))
