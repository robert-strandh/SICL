(cl:in-package :sicl-cons-high)

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

;;;; This file is part of the cons-high module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file cons-high.text for a description of the module.

(defun name-package (name)
  (let ((real-name (if (symbolp name) name (cadr name))))
    (package-name (symbol-package real-name))))

(defmethod cleavir-i18n:report-condition ((c both-test-and-test-not-given)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package):~@
           Both keyword arguments :test and :test-not were given."
	  (name c)
	  (name-package (name c))))

(defmethod cleavir-i18n:report-condition ((c must-be-nonnegative-integer)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package):~@
           A nonnegative integer was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c must-be-cons)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package):~@
           A cons cell was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c must-be-list)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package):~@
           A list (a cons or nil) was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c must-be-proper-list)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package):~@
           A proper list was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c must-be-proper-or-circular-list)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package):~@
           A proper or circular list was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c must-be-proper-or-dotted-list)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package):~@
           A proper or dotted list was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c must-be-property-list)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package):~@
           A property list was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c must-be-association-list)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package):~@
           A association list was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c at-least-one-list-required)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package):~@
           At least one list argument is required,~@
           but none was given."
	  (name c)
	  (name-package (name c))))
	  
(defmethod cleavir-i18n:report-condition ((c at-least-one-argument-required)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package):~@
           At least one argument is required,~@
           but none was given."
	  (name c)
	  (name-package (name c))))

(defmethod cleavir-i18n:report-condition ((c lists-must-have-the-same-length)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package):~@
           The two lists passed as arguments must~@
           have the same length, but the following~@
           was given:~@
           ~s~@
           and~@
           ~s."
	  (name c)
	  (name-package (name c))
	  (list1 c)
	  (list2 c)))

(defmethod cleavir-i18n:report-condition ((c setf-c*r-must-be-cons)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In the SETF expander for ~a (in the ~a package),~@
           the ~aargument ~s~@
           must be a cons cell, but the following was given instead:~@
           ~s."
	  (name c)
	  (name-package (name c))
	  (if (zerop (length (access-string c)))
	      ""
	      (format nil "C~aR of the " (access-string c)))
	  (original-tree c)
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c setf-nth-must-be-cons)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In the SETF expander for ~a (in the ~a package),~@
           the ~:R CDR of the argument ~s~@
           must be a CONS cell, but the following was given instead:~@
           ~s."
	  (name c)
	  (name-package (name c))
	  (cons-cell-count c)
	  (original-tree c)
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c warn-both-test-and-test-not-given)
					  stream
					  (language cleavir-i18n:english))
  (format stream
	  "In ~a (in the ~a package),~@
           both keyword arguments :test and :test-not were given."
	  (name c)
	  (name-package (name c))))

(defmethod cleavir-i18n:report-condition
    ((c expected-list-with-at-least-n-elements)
     stream
     (language cleavir-i18n:english))
  (format stream
	  "Expected a list with at least ~d elements,~@
           but the following was given instead:~@
           ~s."
	  (at-least c)
	  (found c)))
