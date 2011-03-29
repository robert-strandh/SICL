;;;; Copyright (c) 2008, 2009, 2010, 2011
;;;;
;;;;     Robert Strandh (strandh@labri.fr)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the conditionals module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file conditionals.text for a description of the module.

(in-package :sicl-conditionals)

(defparameter *language* 'en-us)

(defun name-package (name)
  (let ((real-name (if (symbolp name) name (cadr name))))
    (package-name (symbol-package real-name))))

(defgeneric report-condition (condition stream language))

(defmethod print-object ((c name-mixin) stream)
  (report-condition c stream *language*))

(defmethod report-condition ((condition malformed-body)
			     stream
			     (language (eql 'en-us)))
  (princ "Expected a proper list of forms," stream)
  (terpri stream)
  (princ "but found: " stream)
  (print (body condition) stream))
     
(defmethod report-condition ((condition malformed-cond-clauses)
			     stream
			     (language (eql 'en-us)))
  (princ "Expected a proper list of cond clauses," stream)
  (terpri stream)
  (princ "but found: " stream)
  (print (clauses condition) stream))
     
(defmethod report-condition ((condition malformed-cond-clause)
			     stream
			     (language (eql 'en-us)))
  (princ "Expected a cond clause of the form," stream)
  (terpri stream)
  (princ "(test-form form*)," stream)
  (terpri stream)
  (princ "but found: " stream)
  (print (clause condition) stream))
     
(defmethod report-condition ((condition malformed-case-clauses)
			     stream
			     (language (eql 'en-us)))
  (princ "Expected a proper list of case clauses," stream)
  (terpri stream)
  (princ "but found: " stream)
  (print (clauses condition) stream))
     
(defmethod report-condition ((condition malformed-case-clause)
			     stream
			     (language (eql 'en-us)))
  (princ "Expected a case clause of the form," stream)
  (terpri stream)
  (princ "(keys form*)," stream)
  (terpri stream)
  (princ "but found: " stream)
  (print (clause condition) stream))
     
(defmethod report-condition ((condition otherwise-clause-not-last)
			     stream
			     (language (eql 'en-us)))
  (princ "The `otherwise' or `t' clause must be last in a case form," stream)
  (terpri stream)
  (princ "but but it was followed by: " stream)
  (print (clauses condition) stream))

(defmethod report-condition ((condition malformed-keys)
			     stream
			     (language (eql 'en-us)))
  (princ "Expected a designator for a list of keys," stream)
  (terpri stream)
  (princ "but found: " stream)
  (print (keys condition) stream))
     
(defmethod report-condition ((condition malformed-typecase-clauses)
			     stream
			     (language (eql 'en-us)))
  (princ "Expected a proper list of typecase clauses," stream)
  (terpri stream)
  (princ "but found: " stream)
  (print (clauses condition) stream))
     
(defmethod report-condition ((condition malformed-typecase-clause)
			     stream
			     (language (eql 'en-us)))
  (princ "Expected a typecase clause of the form," stream)
  (terpri stream)
  (princ "(type form*)," stream)
  (terpri stream)
  (princ "but found: " stream)
  (print (clause condition) stream))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at runtime

(defmethod report-condition ((condition ecase-type-error)
			     stream
			     (language (eql 'en-us)))
  (princ "No key matched in ecase expression." stream)
  (terpri stream)
  (princ "Offending datum: " stream)
  (print (type-error-datum condition) stream)
  (princ "Offending datum: " stream)
  (print (type-error-expected-type condition) stream))

(defmethod report-condition ((condition ccase-type-error)
			     stream
			     (language (eql 'en-us)))
  (princ "No key matched in ccase expression." stream)
  (terpri stream)
  (princ "Offending datum: " stream)
  (print (type-error-datum condition) stream)
  (princ "Offending datum: " stream)
  (print (type-error-expected-type condition) stream))

(defmethod report-condition ((condition etypecase-type-error)
			     stream
			     (language (eql 'en-us)))
  (princ "No key matched in etypecase expression." stream)
  (terpri stream)
  (princ "Offending datum: " stream)
  (print (type-error-datum condition) stream)
  (princ "Offending datum: " stream)
  (print (type-error-expected-type condition) stream))

(defmethod report-condition ((condition ctypecase-type-error)
			     stream
			     (language (eql 'en-us)))
  (princ "No key matched in ctypecase expression." stream)
  (terpri stream)
  (princ "Offending datum: " stream)
  (print (type-error-datum condition) stream)
  (princ "Offending datum: " stream)
  (print (type-error-expected-type condition) stream))

