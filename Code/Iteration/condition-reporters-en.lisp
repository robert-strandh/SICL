(cl:in-package #:sicl-iteration)

(defun name-package (name)
  (let ((real-name (if (symbolp name) name (cadr name))))
    (package-name (symbol-package real-name))))

;;; I used to think that we should not use FORMAT for these condition
;;; reporters.  I now think it is OK.  RS -- 2014-09-15.

(defmethod acclimation:report-condition ((c malformed-binding-var)
					  stream
					  (language acclimation:english))
  (format stream
	  "In ~a (in the ~a package):~@
           The binding variable must be a symbol,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
          (type-error-datum c)))

(defmethod acclimation:report-condition ((c malformed-list-form)
					  stream
					  (language acclimation:english))
  (format stream
	  "In ~a (in the ~a package):~@
           The list form must be a list,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod acclimation:report-condition ((c malformed-count-form)
					  stream
					  (language acclimation:english))
  (format stream
	  "In ~a (in the ~a package):~@
           The count form must be a non-negative integer,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod acclimation:report-condition ((c malformed-body)
					  stream
					  (language acclimation:english))
  (format stream
	  "In ~a (in the ~a package):~@
           The body must be a proper list,~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod acclimation:report-condition ((c malformed-variable-clauses)
					  stream
					  (language acclimation:english))
  (format stream
	  "In ~a (in the ~a package):~@
           Expected a proper list of variable clauses~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (type-error-datum c)))

(defmethod acclimation:report-condition ((c malformed-variable-clause)
					  stream
					  (language acclimation:english))
  (format stream
	  "In ~a (in the ~a package):~@
           Expected a variable clause of the form~@
           var, (var), (var init-form), or (var init-form step-form),~@
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (found c)))

(defmethod acclimation:report-condition ((c malformed-end-test)
					  stream
					  (language acclimation:english))
  (format stream
	  "In ~a (in the ~a package):~@
           Expected an end test clause of the form~@
           (end-test result-form*),
           but the following was given:~@
           ~s"
	  (name c)
	  (name-package (name c))
	  (found c)))
