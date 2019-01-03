(cl:in-package :sicl-sequence)

(defparameter *language* 'en-us)

(defun name-package (name)
  (let ((real-name (if (symbolp name) name (cadr name))))
    (package-name (symbol-package real-name))))

(defgeneric report-condition (condition stream language))

(defmethod print-object ((c name-mixin) stream)
  (report-condition c stream *language*))

(defmethod report-condition  ((c both-test-and-test-not-given)
                              stream
                              (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package):~@
           Both keyword arguments :test and :test-not were given."
          (name c)
          (name-package (name c))))

(defmethod report-condition ((c must-be-nonnegative-integer)
                             stream
                             (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package):~@
           A nonnegative integer was required,~@
           but the following was given:~@
           ~s"
          (name c)
          (name-package (name c))
          (type-error-datum c)))

(defmethod report-condition ((c must-be-cons)
                             stream
                             (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package):~@
           A cons cell was required,~@
           but the following was given:~@
           ~s"
          (name c)
          (name-package (name c))
          (type-error-datum c)))

(defmethod report-condition ((c must-be-list)
                             stream
                             (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package):~@
           A list (a cons or nil) was required,~@
           but the following was given:~@
           ~s"
          (name c)
          (name-package (name c))
          (type-error-datum c)))

(defmethod report-condition ((c must-be-proper-list)
                             stream
                             (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package):~@
           A proper list was required,~@
           but the following was given:~@
           ~s"
          (name c)
          (name-package (name c))
          (type-error-datum c)))

(defmethod report-condition ((c warn-both-test-and-test-not-given)
                             stream
                             (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package),~@
           both keyword arguments :test and :test-not were given."
          (name c)
          (name-package (name c))))

(defmethod report-condition ((c invalid-sequence-index-type)
                             stream
                             (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package),~@
           The value ~s is not a valid type for a sequence index."
          (name c)
          (name-package (name c))
          (type-error-datum c)))

(defmethod report-condition ((c invalid-start-index-type)
                             stream
                             (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package),~@
           The value ~s is not a valid type for a sequence start index."
          (name c)
          (name-package (name c))
          (type-error-datum c)))

(defmethod report-condition ((c invalid-end-index-type)
                             stream
                             (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package),~@
           The value ~s is not a valid type for a sequence end index."
          (name c)
          (name-package (name c))
          (type-error-datum c)))

(defmethod report-condition ((c invalid-sequence-index)
                             stream
                             (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package),~@
           The value ~s is not a valid sequence index for the sequence:~@
           ~s."
          (name c)
          (name-package (name c))
          (type-error-datum c)
          (in-sequence c)))

(defmethod report-condition ((c invalid-start-index)
                             stream
                             (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package),~@
           The value ~s is not a valid start index for the sequence:~@
           ~s."
          (name c)
          (name-package (name c))
          (type-error-datum c)
          (in-sequence c)))

(defmethod report-condition ((c invalid-end-index)
                             stream
                             (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package),~@
           The value ~s is not a valid end index for the sequence:~@
           ~s."
          (name c)
          (name-package (name c))
          (type-error-datum c)
          (in-sequence c)))

(defmethod report-condition ((c end-less-than-start)
                             stream
                             (language (eql 'en-us)))
  (format stream
          "In ~a (in the ~a package),~@
           The bounding indexes start: ~s and end: ~s are not valid~@
           bounding indexes, because start must be less than or equal~@
           to end.  The sequence is:~@
           ~s."
          (name c)
          (name-package (name c))
          (type-error-datum c)
          (end-index c)
          (in-sequence c)))
