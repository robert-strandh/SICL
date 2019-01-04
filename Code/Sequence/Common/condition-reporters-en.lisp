(cl:in-package #:sicl-sequence)

(defmethod acclimation:report-condition
    ((c both-test-and-test-not-given)
     stream
     (language (eql 'en-us)))
  (format stream
          "Both keyword arguments :test and :test-not were given."))

(defmethod acclimation:report-condition
    ((c must-be-nonnegative-integer)
     stream
     (language (eql 'en-us)))
  (format stream
          "A nonnegative integer was required, ~
           but the following was given:~@
           ~s"
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c must-be-cons)
     stream
     (language (eql 'en-us)))
  (format stream
          "A cons cell was required, ~
           but the following was given:~@
           ~s"
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c must-be-list)
     stream
     (language (eql 'en-us)))
  (format stream
          "A list (a cons or nil) was required, ~
           but the following was given:~@
           ~s"
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c must-be-proper-list)
     stream
     (language (eql 'en-us)))
  (format stream
          "A proper list was required, ~
           but the following was given:~@
           ~s"
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c invalid-sequence-index-type)
     stream
     (language (eql 'en-us)))
  (format stream
          "The value ~s is not a valid type for a sequence index."
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c invalid-start-index-type)
     stream
     (language (eql 'en-us)))
  (format stream
          "The value ~s is not a valid type for a sequence start index."
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c invalid-end-index-type)
     stream
     (language (eql 'en-us)))
  (format stream
          "The value ~s is not a valid type for a sequence end index."
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c invalid-sequence-index)
     stream
     (language (eql 'en-us)))
  (format stream
          "The value ~s is not a valid sequence index for the sequence:~@
           ~s."
          (type-error-datum c)
          (in-sequence c)))

(defmethod acclimation:report-condition
    ((c invalid-start-index)
     stream
     (language (eql 'en-us)))
  (format stream
          "The value ~s is not a valid start index for the sequence:~@
           ~s."
          (type-error-datum c)
          (in-sequence c)))

(defmethod acclimation:report-condition
    ((c invalid-end-index)
     stream
     (language (eql 'en-us)))
  (format stream
          "The value ~s is not a valid end index for the sequence:~@
           ~s."
          (type-error-datum c)
          (in-sequence c)))

(defmethod acclimation:report-condition
    ((c end-less-than-start)
     stream
     (language (eql 'en-us)))
  (format stream
          "The bounding indexes start: ~s and end: ~s are not valid~@
           bounding indexes, because start must be less than or equal~@
           to end.  The sequence is:~@
           ~s."
          (type-error-datum c)
          (end-index c)
          (in-sequence c)))
