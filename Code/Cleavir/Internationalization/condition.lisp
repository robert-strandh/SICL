(cl:in-package #:cleavir-internationalization)

(defgeneric report-condition (condition stream language))

(define-condition condition ()
  ()
  (:report (lambda (condition stream)
	     (report-condition condition stream (language *locale*)))))
