(cl:in-package #:cleavir-internationalization)

(defclass english (language)
  ())

(defmethod long-day-name ((day (eql 1)) (language english))
  "Monday")

(defmethod long-day-name ((day (eql 2)) (language english))
  "Tuesday")

(defmethod long-day-name ((day (eql 3)) (language english))
  "Wednesday")

(defmethod long-day-name ((day (eql 4)) (language english))
  "Thursday")

(defmethod long-day-name ((day (eql 5)) (language english))
  "Friday")

(defmethod long-day-name ((day (eql 6)) (language english))
  "Saturday")

(defmethod long-day-name ((day (eql 7)) (language english))
  "Sunday")
