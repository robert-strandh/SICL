(cl:in-package #:sicl-sequence)

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
    ((c must-be-sequence)
     stream
     (language (eql 'en-us)))
  (format stream
          "A sequence was required, ~
           but the following was given:~@
           ~s"
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c must-be-function-designator)
     stream
     (language (eql 'en-us)))
  (format stream
          "A function designator was required, ~
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
    ((c must-be-recognizable-subtype-of-sequence)
     stream
     (language (eql 'en-us)))
  (format stream
          "A recognizable subtype of sequence was required, ~
           but the following was given:~@
           ~s"
          (type-error-datum c)))

;;; Index Handling

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
          "The value ~s is not a valid sequence start index."
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c invalid-end-index)
     stream
     (language (eql 'en-us)))
  (format stream
          "The value ~s is not a valid sequence end index."
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c end-less-than-start)
     stream
     (language (eql 'en-us)))
  (format stream
          "The bounding indexes start: ~s and end: ~s are not valid~@
           bounding indexes, because start must be less than or equal~@
           to end.  The sequence is:~@
           ~s."
          (start-index c)
          (type-error-datum c)
          (in-sequence c)))

(defmethod acclimation:report-condition
    ((c both-test-and-test-not-given)
     stream
     (language (eql 'en-us)))
  (format stream
          "Both keyword arguments :test and :test-not were given."))
