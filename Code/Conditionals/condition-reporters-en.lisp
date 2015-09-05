;;;; Copyright (c) 2008 - 2013
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

;;;; This file is part of the conditionals module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file conditionals.text for a description of the module.

(in-package :sicl-conditionals)

(defmethod cleavir-i18n:report-condition ((condition malformed-body)
					  stream
					  (language cleavir-i18n:english))
  (format stream
          "Expected a proper list of forms,~@
           but the following was given instead:~@
           ~s"
	  (body condition)))

(defmethod cleavir-i18n:report-condition ((condition malformed-cond-clauses)
					  stream
					  (language cleavir-i18n:english))
  (format stream
          "Expected a proper list of cond clauses,~@
           but the following was given instead:~@
           ~s"
	  (clauses condition)))

(defmethod cleavir-i18n:report-condition ((condition malformed-cond-clause)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "Expected a cond clause of the form,~@
           (test-form form*),~@
           but the following was given instead:~@
           ~s"
	  (clause condition)))

(defmethod cleavir-i18n:report-condition ((condition malformed-case-clauses)
					  stream
					  (language cleavir-i18n:english))
  (format stream
          "Expected a proper list of case clauses,~@
           but the following was given instead:~@
           ~s"
	  (clauses condition)))

(defmethod cleavir-i18n:report-condition ((condition malformed-case-clause)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "Expected a case clause of the form,~@
           (keys form*),~@
           but the following was given instead:~@
           ~s"
	  (clause condition)))

(defmethod cleavir-i18n:report-condition ((condition otherwise-clause-not-last)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "The `otherwise' or `t' clause must be last in a case form,~@
           but but it was followed by:~@
           ~s"
	  (clauses condition)))

(defmethod cleavir-i18n:report-condition ((condition malformed-keys)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "Expected a designator for a list of keys,~@
           but the following was given instead:~@
           ~s"
	  (keys condition)))

(defmethod cleavir-i18n:report-condition ((condition malformed-typecase-clauses)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "Expected a proper list of typecase clauses,~@
           but the following was given instead:~@
           ~s"
	  (clauses condition)))

(defmethod cleavir-i18n:report-condition ((condition malformed-typecase-clause)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "Expected a typecase clause of the form,~@
	  (type form*),~@
           but the following was given instead:~@
           ~s"
	  (clause condition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at runtime

(defmethod cleavir-i18n:report-condition ((condition ecase-type-error)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "No key matched in ecase expression.~@
           Offending datum:~@
           ~s~@
           Offending type:~@
           ~s"
          (type-error-datum condition)
	  (type-error-expected-type condition)))

(defmethod cleavir-i18n:report-condition ((condition ccase-type-error)
					  stream
					  (language cleavir-i18n:english))
  (princ "No key matched in ccase expression." stream)
  (terpri stream)
  (princ "Offending datum: " stream)
  (print (type-error-datum condition) stream)
  (princ "Offending datum: " stream)
  (print (type-error-expected-type condition) stream))

(defmethod cleavir-i18n:report-condition ((condition etypecase-type-error)
					  stream
					  (language cleavir-i18n:english))
  (princ "No key matched in etypecase expression." stream)
  (terpri stream)
  (princ "Offending datum: " stream)
  (print (type-error-datum condition) stream)
  (princ "Offending datum: " stream)
  (print (type-error-expected-type condition) stream))

(defmethod cleavir-i18n:report-condition ((condition ctypecase-type-error)
					  stream
					  (language cleavir-i18n:english))
  (princ "No key matched in ctypecase expression." stream)
  (terpri stream)
  (princ "Offending datum: " stream)
  (print (type-error-datum condition) stream)
  (princ "Offending datum: " stream)
  (print (type-error-expected-type condition) stream))

