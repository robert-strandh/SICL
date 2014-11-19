(cl:in-package #:cleavir-internationalization)

(defclass francais (language)
  ())

(defmethod long-day-name ((day (eql 1)) (language francais))
  "Lundi")

(defmethod long-day-name ((day (eql 2)) (language francais))
  "Mardi")

(defmethod long-day-name ((day (eql 3)) (language francais))
  "Mercredi")

(defmethod long-day-name ((day (eql 4)) (language francais))
  "Jeudi")

(defmethod long-day-name ((day (eql 5)) (language francais))
  "Vendredi")

(defmethod long-day-name ((day (eql 6)) (language francais))
  "Samedi")

(defmethod long-day-name ((day (eql 7)) (language francais))
  "Dimanche")
