(cl:in-package #:sicl-additional-conditions)

;;;; Copyright (c) 2008, 2009, 2010, 2012, 2015
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Runtime conditions.

(defun interpret-type (type)
  (cond ((eq type 'cons)
	 "a CONS cell")
	((eq type 'list)
	 "a list (NIL or a CONS cell)")
	((eq type 'proper-list)
	 "a proper list")
	((eq type 'dotted-list)
	 "a dotted list")
	((eq type 'circular-list)
	 "a circular list")
	((equal type '(integer 0))
	 "a non-negative integer")
	(t
	 (format nil "an object of type ~s" type))))

(defmethod cleavir-i18n:report-condition
    ((c sicl-type-error) stream (language cleavir-i18n:english))
  (format stream
	  "Expected ~a.~@
	   But got the following instead:~@
           ~s"
	  (interpret-type (type-error-expected-type c))
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition
    ((c both-test-and-test-not-given) stream (language cleavir-i18n:english))
  (format stream
	  "Both keyword arguments :test and :test-not were given."))

(defmethod cleavir-i18n:report-condition
    ((c at-least-one-list-required) stream (language cleavir-i18n:english))
  (format stream
	  "At least one list argument is required,~@
           but none was given."))
	  
(defmethod cleavir-i18n:report-condition
    ((c at-least-one-argument-required) stream (language cleavir-i18n:english))
  (format stream
	  "At least one argument is required,~@
           but none was given."))
	  
(defmethod cleavir-i18n:report-condition
    ((c lists-must-have-the-same-length) stream (language cleavir-i18n:english))
  (format stream
	  "The two lists passed as arguments must~@
           have the same length, but the following~@
           was given:~@
           ~s~@
           and~@
           ~s."
	  (list1 c)
	  (list2 c)))

(defmethod cleavir-i18n:report-condition
    ((c warn-both-test-and-test-not-given) stream (language cleavir-i18n:english))
  (format stream
	  "Both keyword arguments :test and :test-not were given."))

(defmethod cleavir-i18n:report-condition
    ((c sicl-unbound-variable) stream (language cleavir-i18n:english))
  (format stream
	  "The variable named ~s in unbound."
	  (cell-error-name c)))

(defmethod cleavir-i18n:report-condition
    ((c sicl-undefined-function) stream (language cleavir-i18n:english))
  (format stream
	  "The funcation named ~s in undefined."
	  (cell-error-name c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile time conditions. 

(defmethod cleavir-i18n:report-condition
    ((c form-must-be-proper-list) stream (language cleavir-i18n:english))
  (format stream
	  "A form must be a proper list.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c body-must-be-proper-list) stream (language cleavir-i18n:english))
  (format stream
	  "A code body must be a proper list.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c block-tag-must-be-symbol) stream (language cleavir-i18n:english))
  (format stream
	  "A block tag must be a symbol.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c go-tag-must-be-symbol-or-integer) stream (language cleavir-i18n:english))
  (format stream
	  "A GO tag must be a symbol or an integer.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c multiple-documentation-strings-in-body) stream (language cleavir-i18n:english))
  (format stream
	  "Multiple documentation strings found in code body:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c documentation-string-not-allowed-in-body) stream (language cleavir-i18n:english))
  (format stream
	  "A documentation string was found where none is allowed:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c declarations-not-allowed-in-body) stream (language cleavir-i18n:english))
  (format stream
	  "Declarations found where none is allowed:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c declaration-follows-form-in-body) stream (language cleavir-i18n:english))
  (format stream
	  "Declarations can not follow the forms in a code body:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c form-too-short) stream (language cleavir-i18n:english))
  (format stream
	  "The form:~@
           ~s~@
           should have at least ~a subforms, but has only ~a."
	  (code c)
	  (min-length c)
	  (length (code c))))

(defmethod cleavir-i18n:report-condition
    ((c form-too-long) stream (language cleavir-i18n:english))
  (format stream
	  "The form:~@
           ~s~@
           should have at most ~a subforms, but has ~a."
	  (code c)
	  (max-length c)
	  (length (code c))))

(defmethod cleavir-i18n:report-condition
    ((c unknown-eval-when-situation) stream (language cleavir-i18n:english))
  (format stream
	  "Unknown evaluation situation given:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c deprecated-eval-when-situation) stream (language cleavir-i18n:english))
  (format stream
	  "A deprecated evaluation situation given:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c setq-must-have-even-number-arguments) stream (language cleavir-i18n:english))
  (format stream
	  "An even number of arguments are required.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c setq-variable-must-be-symbol) stream (language cleavir-i18n:english))
  (format stream
	  "A variable assigned to must be a symbol.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c tagbody-element-must-be-symbol-integer-or-compound-form)
     stream
     (language cleavir-i18n:english))
  (format stream
	  "Element must be a symbol, an integer, or a compound form.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c empty-body) stream (language cleavir-i18n:english))
  (format stream
	  "The body of this form is empty:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c numeric-catch-tag) stream (language cleavir-i18n:english))
  (format stream
	  "CATCH tags are compared with EQ so using a numeric~@
           CATCH tag may not work as expected:~@
           ~s"
	  (code c)))

(defmethod cleavir-i18n:report-condition
    ((c load-time-value-read-only-p-not-evaluated)
     stream
     (language cleavir-i18n:english))
  (format stream
	  "The second (optional) argument (read-only-p) is not evaluated,~@
           so a boolean value (T or NIL) was expected.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLOS/MOP-related conditions.

(defmethod cleavir-i18n:report-condition  ((c no-such-class-name)
			      stream
			      (language cleavir-i18n:english))
  (format stream
	  "There is no class with the name ~s."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition  ((c must-be-class-or-nil)
			      stream
			      (language cleavir-i18n:english))
  (format stream
	  "A class object or NIL was expected, but
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c superclass-list-must-be-proper-list)
			     stream
			     (language cleavir-i18n:english))
  (format stream
	  "The list of superclasses must be a proper list, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c class-name-must-be-non-nil-symbol)
			     stream
			     (language cleavir-i18n:english))
  (format stream
	  "A class name must be a non-nil symbol, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c malformed-slots-list)
			     stream
			     (language cleavir-i18n:english))
  (format stream
	  "The direct-slots must be a proper list of slot specs, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c malformed-slot-spec)
			     stream
			     (language cleavir-i18n:english))
  (format stream
	  "Malformed slot specification.~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c illegal-slot-name)
			     stream
			     (language cleavir-i18n:english))
  (format stream
	  "Illegal slot name~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c slot-options-must-be-even)
			     stream
			     (language cleavir-i18n:english))
  (format stream
	  "There must be an even number of slot options.~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c slot-option-name-must-be-symbol)
			     stream
			     (language cleavir-i18n:english))
  (format stream
	  "The name of a slot option must be a symbol, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c multiple-initform-options-not-permitted)
			     stream
			     (language cleavir-i18n:english))
  (format stream
	  "A slot can not have multiple :initform options.~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c multiple-documentation-options-not-permitted)
			     stream
			     (language cleavir-i18n:english))
  (format stream
	  "A slot can not have multiple :documentation options.~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c multiple-allocation-options-not-permitted)
			     stream
			     (language cleavir-i18n:english))
  (format stream
	  "A slot can not have multiple :allocation options.~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c multiple-type-options-not-permitted)
			     stream
			     (language cleavir-i18n:english))
  (format stream
	  "A slot can not have multiple :type options.~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c slot-documentation-option-must-be-string)
			     stream
			     (language cleavir-i18n:english))
  (format stream
	  "The :documentation option of a slot must have a string argument, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c class-option-must-be-non-empty-list)
			     stream
			     (langauge cleavir-i18n:english))
  (format stream
	  "A class option must be a a non-empty list, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c class-option-name-must-be-symbol)
			     stream
			     (langauge cleavir-i18n:english))
  (format stream
	  "A class option name must be a symbol, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c malformed-documentation-option)
			     stream
			     (langauge cleavir-i18n:english))
  (format stream
	  "A documentation option must have the form~@
           (:documentation <name>), but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c malformed-metaclass-option)
			     stream
			     (langauge cleavir-i18n:english))
  (format stream
	  "A documentation option must have the form~@
           (:documentation <name>), but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c malformed-default-initargs-option)
			     stream
			     (langauge cleavir-i18n:english))
  (format stream
	  "The DEFAULT-INITARG option takes the form~@
           (:default-initargs <name> <value> <name> <value>...), but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c default-initargs-option-once)
			     stream
			     (langauge cleavir-i18n:english))
  (format stream
	  "The default-initargs option can appear only once in the~@
           list of class options, but a second such option:~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c documentation-option-once)
			     stream
			     (langauge cleavir-i18n:english))
  (format stream
	  "The documentation option can appear only once in the~@
           list of class options, but a second such option:~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c metaclass-option-once)
			     stream
			     (langauge cleavir-i18n:english))
  (format stream
	  "The metaclass option can appear only once in the~@
           list of class options, but a second such option:~@
           ~s was found."
	  (type-error-datum c)))

(defmethod cleavir-i18n:report-condition ((c unknown-class-option)
			     stream
			     (langauge cleavir-i18n:english))
  (format stream
	  "A class option is either ~@
           :default-initargs, :documentation, or :metaclass, but~@
           ~s was found."
	  (type-error-datum c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Argument mismatch conditions.

(defmethod cleavir-i18n:report-condition
    ((c too-few-arguments) stream (language cleavir-i18n:english))
  (format stream
	  "Too few arguments were given.  The lambda list is:~@
           ~s~@
           and the arguments given were:~@
           ~s"
	  (lambda-list c)
	  (arguments c)))

(defmethod cleavir-i18n:report-condition
    ((c too-many-arguments) stream (language cleavir-i18n:english))
  (format stream
	  "Too many arguments were given.  The lambda list is:~@
           ~s~@
           and the arguments given were:~@
           ~s"
	  (lambda-list c)
	  (arguments c)))

(defmethod cleavir-i18n:report-condition
    ((c unrecognized-keyword-argument) stream (language cleavir-i18n:english))
  (format stream
	  "The keyword argument:~@
           ~s~@
           Is not recognized by this function.  The lambda list is:~@
           ~s~@
           and the arguments given were:~@
           ~s"
	  (keyword-argument c)
	  (lambda-list c)
	  (arguments c)))

