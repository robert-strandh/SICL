(in-package :sicl-cons-high)

(defparameter *language* 'en-us)

(defgeneric report-condition (condition stream language))

(defmethod print-object ((c name-mixin) stream)
  (report-condition c stream *language*))

(defmethod report-condition  ((c both-test-and-test-not-given)
			      stream
			      (language (eql 'en-us)))
  (format stream
	  "In ~a (a symbol of the ~a package):~@
           Both keyword arguments :test and :test-not were given."
	  (name c)
	  (package-name (symbol-package (name c)))))

(defmethod report-condition ((c must-be-nonnegative-integer)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (a symbol of the ~a package):~@
           A nonnegative integer was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (package-name (symbol-package (name c)))
	  (type-error-datum c)))

(defmethod report-condition ((c must-be-list)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (a symbol of the ~a package):~@
           A list (a cons or nil) was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (package-name (symbol-package (name c)))
	  (type-error-datum c)))

(defmethod report-condition ((c must-be-proper-list)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (a symbol of the ~a package):~@
           A proper list was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (package-name (symbol-package (name c)))
	  (type-error-datum c)))

(defmethod report-condition ((c must-be-proper-or-circular-list)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (a symbol of the ~a package):~@
           A proper or circular list was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (package-name (symbol-package (name c)))
	  (type-error-datum c)))

(defmethod report-condition ((c must-be-property-list)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (a symbol of the ~a package):~@
           A property list was required,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (package-name (symbol-package (name c)))
	  (type-error-datum c)))

(defmethod report-condition ((c at-least-one-list-required)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (a symbol of the ~a package):~@
           At least one list argument is required,~@
           but none was given."
	  (name c)
	  (package-name (symbol-package (name c)))))
	  
(defmethod report-condition ((c at-least-one-argument-required)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (a symbol of the ~a package):~@
           At least one argument is required,~@
           but none was given."
	  (name c)
	  (package-name (symbol-package (name c)))))
	  