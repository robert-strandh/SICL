(cl:in-package #:sicl-format)

(defun report-control-string-and-directive-start-position (condition stream)
  (format stream
	  "In the control-string \"~a\",~%~
           in the directive that starts at position ~a,~%"
	  (control-string condition)
	  (tilde-position condition)))

(defun report-control-string-and-directive-position (condition stream)
  (with-accessors ((control-string control-string)
		   (start start)
		   (end end))
    (directive condition)
    (format stream
	    "In the control-string \"~a\",~%in the directive that ~
             starts at position ~a and ends at position ~a,~%"
	    control-string
	    start
	    end)))

(defmethod cleavir-i18n:report-condition
    ((condition end-of-control-string-error)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-start-position condition stream)
  (format stream
	  "~a, but reached the end of the control string"
	  (why condition)))

(defmethod cleavir-i18n:report-condition
    ((condition expected-integer-error)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-start-position condition stream)
  (format stream
	  "expected an integer at index ~a,~% but found the character `~a' instead"
	  (index condition)
	  (char (control-string condition) (index condition))))

(defmethod cleavir-i18n:report-condition
    ((condition expected-parameter-start)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-start-position condition stream)
  (format stream
	  "expected one of ', +, -, or a decimal digit at index ~a,~%~
              but found the character `~a' instead"
	  (index condition)
	  (char (control-string condition) (index condition))))

(defmethod cleavir-i18n:report-condition
    ((condition two-identical-modifiers)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-start-position condition stream)
  (format stream
	  "found two identical modifiers `~a' at index ~a"
	  (char (control-string condition) (index condition))
	  (index condition)))

(defmethod cleavir-i18n:report-condition
    ((condition more-than-two-modifiers)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-start-position condition stream)
  (format stream
	  "found a sequence of more than two modifiers at index ~a"
	  (index condition)))

(defmethod cleavir-i18n:report-condition
    ((condition unknown-format-directive)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-start-position condition stream)
  (format stream
	  "unknown format directive `~a' at index ~a"
	  (char (control-string condition) (index condition))
	  (index condition)))

(defmethod cleavir-i18n:report-condition
    ((condition unknown-directive-character)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-position condition stream)
  (format stream
	  "unknown directive character: ~c~%"
	  (directive-character (directive condition))))

;;; FIXME, report the index
(defmethod cleavir-i18n:report-condition
    ((condition directive-takes-no-modifiers)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-position condition stream)
  (format stream
	  "found a modifier at index,~%but this ~
           directive takes no modifiers"))

;;; FIXME, report the index
(defmethod cleavir-i18n:report-condition
    ((condition directive-takes-only-colon)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-position condition stream)
  (format stream
	  "found an at-sign at index,~%but this directive ~
           takes only the colon modifier"))

;;; FIXME, report the index
(defmethod cleavir-i18n:report-condition
    ((condition directive-takes-at-most-one-modifier)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-position condition stream)
  (format stream
	  "found both modifiers,~%but this directive ~
           takes at most one modifier"))
