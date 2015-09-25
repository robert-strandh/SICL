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
    ((condition directive-takes-only-at-sign)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-position condition stream)
  (format stream
	  "found a colon at index,~%but this directive ~
           takes only the at-sign modifier"))

;;; FIXME, report the index
(defmethod cleavir-i18n:report-condition
    ((condition directive-takes-at-most-one-modifier)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-position condition stream)
  (format stream
	  "found both modifiers,~%but this directive ~
           takes at most one modifier"))

(defmethod cleavir-i18n:report-condition
    ((condition too-many-parameters)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-position condition stream)
  (format stream
	  "the directive takes at most ~a parameters,~%but ~a found"
	  (at-most-how-many condition)
	  (how-many-found condition)))

(defmethod cleavir-i18n:report-condition
    ((condition parameter-type-error)
     stream
     (language cleavir-i18n:english))
  (format stream
	  "~a was required as parameter, but ~a was found"
	  (type-name (type-error-expected-type condition))
	  (type-error-datum condition)))

(defmethod cleavir-i18n:report-condition
    ((condition no-more-arguments)
     stream
     (language cleavir-i18n:english))
  (format stream "an attempt was made to access more arguments than available"))

(defmethod cleavir-i18n:report-condition
    ((condition argument-type-error)
     stream
     (language cleavir-i18n:english))
  (format stream
	  "~a was required as argument, but ~a was found"
	  (type-name (type-error-expected-type condition))
	  (type-error-datum condition)))

(defmethod cleavir-i18n:report-condition
    ((condition too-many-package-markers)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-position condition stream)
  (format stream "the function name contains too many package markers"))

(defmethod cleavir-i18n:report-condition
    ((condition no-such-package)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-position condition stream)
  (format stream "the named package does not exist"))

(defmethod cleavir-i18n:report-condition
    ((condition no-such-symbol)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-position condition stream)
  (format stream "a symbol with that name does not exist"))

(defmethod cleavir-i18n:report-condition
    ((condition symbol-not-external)
     stream
     (language cleavir-i18n:english))
  (report-control-string-and-directive-position condition stream)
  (format stream "the symbol is not external in the package"))

(defmethod cleavir-i18n:report-condition
    ((condition go-to-out-of-bounds)
     stream
     (language cleavir-i18n:english))
  (format stream "an attempt was made to go to argument number ~d ~
                  instead of one between 0 and ~d"
	  (what-argument condition)
	  (max-arguments condition)))

(defmethod cleavir-i18n:report-condition
    ((condition modifier-and-parameter)
     stream
     (language cleavir-i18n:english))
  (format stream "a parameter can be used only of there are no modifiers"))

(defmethod cleavir-i18n:report-condition
    ((condition illegal-clause-separators)
     stream
     (language cleavir-i18n:english))
  (format stream "at most the last clause separator can have ~
                  a `:' modifier"))

(defmethod cleavir-i18n:report-condition
    ((condition clause-separator-with-colon-modifier-not-allowed)
     stream
     (language cleavir-i18n:english))
  (format stream "a default clause is incompatible with modifiers"))

(defmethod cleavir-i18n:report-condition
    ((condition at-least-one-item-required)
     stream
     (language cleavir-i18n:english))
  (format stream "there must be at least one clause in a ~
                  conditional directive"))

(defmethod cleavir-i18n:report-condition
    ((condition colon-modifier-requires-two-clauses)
     stream
     (language cleavir-i18n:english))
  (format stream "a colon modifier requires two clauses"))

(defmethod cleavir-i18n:report-condition
    ((condition at-sign-modifier-requires-one-clause)
     stream
     (language cleavir-i18n:english))
  (format stream "an at-sign modifier requires a single clause"))


