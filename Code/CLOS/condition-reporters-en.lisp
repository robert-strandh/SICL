(in-package :sicl-clos

;;;; Copyright (c) 2011
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

;;;; This file is part of the clos module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file clos.text for a description of the module.

(defparameter *language* 'en-us)

(defun name-package (name)
  (let ((real-name (if (symbolp name) name (cadr name))))
    (package-name (symbol-package real-name))))

(defgeneric report-condition (condition stream language))

(defmethod print-object ((c name-mixin) stream)
  (report-condition c stream *language*))

(defmethod report-condition  ((c no-such-class-name)
			      stream
			      (language (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           There is no class with the name ~s."
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition  ((c must-be-class-or-nil)
			      stream
			      (language (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           A class object or NIL was expected, but
           ~s was found."
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c superclass-list-must-be-proper-list)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           The list of superclasses must be a proper list, but~@
           ~s was found."
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c class-name-must-be-non-nil-symbol)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           A class name must be a non-nil symbol, but~@
           ~s was found."
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-slots-list)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           The direct-slots must be a proper list of slot specs, but
           ~s was found."
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-class-option)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           A class option must be either~@
           (:default-initargs <name> <value> <name> <value>...),~@
           (:documentation <string>), or~@
           (:metaclass <name>), but~@
           ~s was found."
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-documentation-option)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           A documentation option must have the form~@
           (:documentation <name>), but~@
           ~s was found."
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-metaclass-option)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           A documentation option must have the form~@
           (:documentation <name>), but~@
           ~s was found."
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-default-initargs-option)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           The DEFAULT-INITARG option takes the form~@
           (:default-initargs <name> <value> <name> <value>...), but~@
           ~s was found."))

(defmethod report-condition ((c default-initarg-option-once)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           The default-initargs option can appear only once in the~@
           list of class options, but a second such option:~@
           ~s was found."
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c documentation-option-once)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           The documentation option can appear only once in the~@
           list of class options, but a second such option:~@
           ~s was found."
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c metaclass-option-once)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           The metaclass option can appear only once in the~@
           list of class options, but a second such option:~@
           ~s was found."
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c unknown-class-option)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           A class option is either ~@
           :default-initargs, :documentation, or :metaclass, but~@
           ~s was found."
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))
