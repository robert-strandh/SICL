(in-package #:sicl-iteration)

(defparameter *language* 'en-us)

(defun name-package (name)
  (let ((real-name (if (symbolp name) name (cadr name))))
    (package-name (symbol-package real-name))))

(defgeneric report-condition (condition stream language))

(defmethod print-object ((c name-mixin) stream)
  (report-condition c stream *language*))

;;; I used to think that we should not use FORMAT for these condition
;;; reporters.  I now think it is OK.  RS -- 2014-09-15.

(defmethod report-condition  ((c malformed-binding-var)
			      stream
			      (language (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           The binding variable must be a symbol,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
          (type-error-datum c)))

(defmethod report-condition ((c malformed-list-form)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           The list form must be a list,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-count-form)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           The count form must be a non-negative integer,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-body)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           The body must be a proper list,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-variable-clauses)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           Expected a proper list of variable clauses~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-variable-clause)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           Expected a variable clause of the form~@
           var, (var), (var init-form), or (var init-form step-form),~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (found c)))

(defmethod report-condition ((c malformed-end-test)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "In ~a (in the ~a package):~@
           Expected an end test clause of the form~@
           (end-test result-form*),
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (found c)))
