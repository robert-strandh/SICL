(in-package #:sicl-additional-conditions)

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

(defun name-package (name)
  (let ((real-name (if (symbolp name) name (cadr name))))
    (package-name (symbol-package real-name))))

(defgeneric report-condition (condition stream language))

(defmethod print-object ((c sicl-condition) stream)
  (report-condition c stream (language *locale*)))

(defmethod report-condition :before
    ((c signaler-mixin)
     stream
     (language (eql 'en-us)))
  (unless (null (signaler c))
    (format stream
	    "In ~a (in the ~a package):~%"
	    (signaler c)
	    (name-package (signaler c)))))

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

(defmethod report-condition
    ((c sicl-type-error) stream (language (eql 'en-us)))
  (format stream
	  "Expected ~a.~@
	   But got the following instead:~@
           ~s"
	  (interpret-type (type-error-expected-type c))
	  (type-error-datum c)))

(defmethod report-condition
    ((c both-test-and-test-not-given) stream (language (eql 'en-us)))
  (format stream
	  "Both keyword arguments :test and :test-not were given."))

(defmethod report-condition
    ((c at-least-one-list-required) stream (language (eql 'en-us)))
  (format stream
	  "At least one list argument is required,~@
           but none was given."))
	  
(defmethod report-condition
    ((c at-least-one-argument-required) stream (language (eql 'en-us)))
  (format stream
	  "At least one argument is required,~@
           but none was given."))
	  
(defmethod report-condition
    ((c lists-must-have-the-same-length) stream (language (eql 'en-us)))
  (format stream
	  "The two lists passed as arguments must~@
           have the same length, but the following~@
           was given:~@
           ~s~@
           and~@
           ~s."
	  (list1 c)
	  (list2 c)))

(defmethod report-condition
    ((c warn-both-test-and-test-not-given) stream (language (eql 'en-us)))
  (format stream
	  "Both keyword arguments :test and :test-not were given."))

(defmethod report-condition
    ((c sicl-unbound-variable) stream (language (eql 'en-us)))
  (format stream
	  "The variable named ~s in unbound."
	  (cell-error-name c)))

(defmethod report-condition
    ((c sicl-undefined-function) stream (language (eql 'en-us)))
  (format stream
	  "The funcation named ~s in undefined."
	  (cell-error-name c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile time conditions. 

(defmethod report-condition
    ((c form-must-be-proper-list) stream (language (eql 'en-us)))
  (format stream
	  "A form must be a proper list.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c body-must-be-proper-list) stream (language (eql 'en-us)))
  (format stream
	  "A code body must be a proper list.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c block-tag-must-be-symbol) stream (language (eql 'en-us)))
  (format stream
	  "A block tag must be a symbol.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c go-tag-must-be-symbol-or-integer) stream (language (eql 'en-us)))
  (format stream
	  "A GO tag must be a symbol or an integer.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c multiple-documentation-strings-in-body) stream (language (eql 'en-us)))
  (format stream
	  "Multiple documentation strings found in code body:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c documentation-string-not-allowed-in-body) stream (language (eql 'en-us)))
  (format stream
	  "A documentation string was found where none is allowed:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c declarations-not-allowed-in-body) stream (language (eql 'en-us)))
  (format stream
	  "Declarations found where none is allowed:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c declaration-follows-form-in-body) stream (language (eql 'en-us)))
  (format stream
	  "Declarations can not follow the forms in a code body:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c form-too-short) stream (language (eql 'en-us)))
  (format stream
	  "The form:~@
           ~s~@
           should have at least ~a subforms, but has only ~a."
	  (code c)
	  (min-length c)
	  (length (code c))))

(defmethod report-condition
    ((c form-too-long) stream (language (eql 'en-us)))
  (format stream
	  "The form:~@
           ~s~@
           should have at most ~a subforms, but has ~a."
	  (code c)
	  (max-length c)
	  (length (code c))))

(defmethod report-condition
    ((c unknown-eval-when-situation) stream (language (eql 'en-us)))
  (format stream
	  "Unknown evaluation situation given:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c deprecated-eval-when-situation) stream (language (eql 'en-us)))
  (format stream
	  "A deprecated evaluation situation given:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c setq-must-have-even-number-arguments) stream (language (eql 'en-us)))
  (format stream
	  "An even number of arguments are required.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c setq-variable-must-be-symbol) stream (language (eql 'en-us)))
  (format stream
	  "A variable assigned to must be a symbol.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c tagbody-element-must-be-symbol-integer-or-compound-form)
     stream
     (language (eql 'en-us)))
  (format stream
	  "Element must be a symbol, an integer, or a compound form.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c empty-body) stream (language (eql 'en-us)))
  (format stream
	  "The body of this form is empty:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c numeric-catch-tag) stream (language (eql 'en-us)))
  (format stream
	  "CATCH tags are compared with EQ so using a numeric~@
           CATCH tag may not work as expected:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c load-time-value-read-only-p-not-evaluated)
     stream
     (language (eql 'en-us)))
  (format stream
	  "The second (optional) argument (read-only-p) is not evaluated,~@
           so a boolean value (T or NIL) was expected.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Lambda list conditions.

(defmethod report-condition
    ((c lambda-list-must-be-list) stream (language (eql 'en-us)))
  (format stream
	  "A lambda list must be a list.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c lambda-list-must-not-be-circular) stream (language (eql 'en-us)))
  (format stream
	  "A lambda list must not be a circular list.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c lambda-list-must-be-proper-list) stream (language (eql 'en-us)))
  (format stream
	  "This lambda list must be a proper list.~@
           But the following was found instead:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c lambda-list-keyword-not-allowed) stream (language (eql 'en-us)))
  (format stream
	  "Lambda list keyword ~s not allowed in this type of lambda list:~@
           ~s"
	  (lambda-list-keyword c)
	  (code c)))

(defmethod report-condition
    ((c suspect-lambda-list-keyword) stream (language (eql 'en-us)))
  (format stream
	  "Suspect lambda list keyword ~s will be treated as an ordinary symbol.~@
           In this lambda list:~@
           ~s"
	  (lambda-list-keyword c)
	  (code c)))

(defmethod report-condition
    ((c lambda-list-keyword-not-allowed-in-dotted-lambda-list)
     stream
     (language (eql 'en-us)))
  (format stream
	  "Lambda list keyword ~s not allowed in a dotted lambda list:~@
           ~s"
	  (lambda-list-keyword c)
	  (code c)))

(defmethod report-condition
    ((c multiple-occurrences-of-lambda-list-keyword) stream (language (eql 'en-us)))
  (format stream
	  "Lambda list keyword ~s appears multiple times in lambda list:~@
           ~s"
	  (lambda-list-keyword c)
	  (code c)))

(defmethod report-condition
    ((c incorrect-keyword-order) stream (language (eql 'en-us)))
  (format stream
	  "Incorrect lambda list keyword order.~@
           The keyword ~s incorrectly appears before the keyword ~s in:~@
           ~s"
	  (lambda-list-keyword1 c)
	  (lambda-list-keyword2 c)
	  (code c)))

(defmethod report-condition
    ((c both-rest-and-body-occur-in-lambda-list) stream (language (eql 'en-us)))
  (format stream
	  "Both &rest and &body may not occur in a lambda list.
           But they do in this lambda list:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c rest/body-must-be-followed-by-variable) stream (language (eql 'en-us)))
  (format stream
	  "The lambda list keyword &rest or &body must be followed by a variable.~@
           But this is not the case in this lambda list:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c atomic-lambda-list-tail-must-be-variable) stream (language (eql 'en-us)))
  (format stream
	  "The atomic tail of a lambda list must be a variable.~@
           But this is not the case in this lambda list:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c whole-must-be-followed-by-variable) stream (language (eql 'en-us)))
  (format stream
	  "The lambda list keyword &whole must be followed by a variable.~@
           But this is not the case in this lambda list:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c whole-must-appear-first) stream (language (eql 'en-us)))
  (format stream
	  "If &whole is used in a lambda list, it must appear first.~@
           But this is not the case in this lambda list:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c whole-must-be-followed-by-variable) stream (language (eql 'en-us)))
  (format stream
	  "The lambda list keyword &whole must be followed by a variable.~@
           But this is not the case in this lambda list:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c environment-must-be-followed-by-variable) stream (language (eql 'en-us)))
  (format stream
	  "The lambda list keyword &environment must be followed by a variable.~@
           But this is not the case in this lambda list:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c environment-can-appear-at-most-once) stream (language (eql 'en-us)))
  (format stream
	  "The lambda list keyword &environment can occur at most once in a lambda list.~@
           But it occurs several times in this lambda list:~@
           ~s"
	  (code c)))

(defmethod report-condition
    ((c malformed-specialized-required) stream (language (eql 'en-us)))
  (format stream
	  "In this type of lambda list, a required parameter must~@
           have one of the following forms:~@
           - var~@
           - (var)~@
           - (var class-name)~@
           - (var (eql form))~@
           where var is a symbol that is not also the name of ~@
           a constant variable, and class-name is a symbol.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod report-condition
    ((c malformed-ordinary-optional) stream (language (eql 'en-us)))
  (format stream
	  "In this type of lambda list, an item following the &optional~@
           lambda-list keyword must have one of the following forms:~@
           - var~@
           - (var)~@
           - (var init-form)~@
           - (var init-form supplied-parameter-p)~@
           where var and supplied-parameter-p are symbols that are not~@
           also names of constant variables.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod report-condition
    ((c malformed-defgeneric-optional) stream (language (eql 'en-us)))
  (format stream
	  "In this type of lambda list, an item following the &optional~@
           lambda-list keyword must have one of the following forms:~@
           - var~@
           - (var)~@
           where var is a symbol that is not~@
           also the name of a constant variable.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod report-condition
    ((c malformed-destructuring-optional) stream (language (eql 'en-us)))
  (format stream
	  "In this type of lambda list, an item following the &optional~@
           lambda-list keyword must have one of the following forms:~@
           - var~@
           - (pattern)~@
           - (pattern init-form)~@
           - (pattern init-form supplied-parameter-p)~@
           where var and supplied-parameter-p are symbols that are not also~@
           names of constant variables, and pattern is a destructuring pattern.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod report-condition
    ((c malformed-ordinary-key) stream (language (eql 'en-us)))
  (format stream
	  "In this type of lambda list, an item following the &key~@
           lambda-list keyword must have one of the following forms:~@
           - var~@
           - (var)~@
           - (var init-form)~@
           - (var init-form supplied-parameter-p)~@
           - ((keyword var))~@
           - ((keyword var) init-form)~@
           - ((keyword var) init-form supplied-parameter-p)~@
           where var and supplied-parameter-p are symbols that are not~@
           also names of constant variables, and keyword is a symbol.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod report-condition
    ((c malformed-defgeneric-key) stream (language (eql 'en-us)))
  (format stream
	  "In this type of lambda list, an item following the &key~@
           lambda-list keyword must have one of the following forms:~@
           - var~@
           - (var)~@
           - ((keyword var))~@
           where var is a symbol that is not~@
           also the name of a constant variable, and keyword is a symbol.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod report-condition
    ((c malformed-destructuring-key) stream (language (eql 'en-us)))
  (format stream
	  "In this type of lambda list, an item following the &key~@
           lambda-list keyword must have one of the following forms:~@
           - var~@
           - (var)~@
           - (var init-form)~@
           - (var init-form supplied-parameter-p)~@
           - ((keyword pattern))~@
           - ((keyword pattern) init-form)~@
           - ((keyword pattern) init-form supplied-parameter-p)~@
           where var and supplied-parameter-p are symbols that are not also~@
           names of constant variables, keyword is a symbol and~@
           pattern is a destructuring pattern.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod report-condition
    ((c malformed-aux) stream (language (eql 'en-us)))
  (format stream
	  "In a lambda list, an item following the &aux~@
           lambda-list keyword must have one of the following forms:~@
           - var~@
           - (var)~@
           - (var init-form)~@
           where var is a symbol that is not~@
           also the name of a constant variable, and keyword is a symbol.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod report-condition
    ((c malformed-destructuring-tree) stream (language (eql 'en-us)))
  (format stream
	  "A destructuring tree can only contain CONS cells and~@
           symbols that are also not names of contstants.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod report-condition
    ((c malformed-lambda-list-pattern) stream (language (eql 'en-us)))
  (format stream
	  "A lambda-list pattern must be either a tree containing only~@
           CONS cells and symbols that are also not names of contstants,~@
           or a list containing lambda-list keywords.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod report-condition
    ((c required-must-be-variable) stream (language (eql 'en-us)))
  (format stream
	  "In this type of lambda list, the required parameter must~@
           be a variable which is also not the name of a constant.~@
           But the following was found instead:~@
           ~s"
          (code c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLOS/MOP-related conditions.

(defmethod report-condition  ((c no-such-class-name)
			      stream
			      (language (eql 'en-us)))
  (format stream
	  "There is no class with the name ~s."
	  (type-error-datum c)))

(defmethod report-condition  ((c must-be-class-or-nil)
			      stream
			      (language (eql 'en-us)))
  (format stream
	  "A class object or NIL was expected, but
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c superclass-list-must-be-proper-list)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "The list of superclasses must be a proper list, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c class-name-must-be-non-nil-symbol)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "A class name must be a non-nil symbol, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-slots-list)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "The direct-slots must be a proper list of slot specs, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-slot-spec)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "Malformed slot specification.~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c illegal-slot-name)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "Illegal slot name~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c slot-options-must-be-even)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "There must be an even number of slot options.~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c slot-option-name-must-be-symbol)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "The name of a slot option must be a symbol, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c multiple-initform-options-not-permitted)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "A slot can not have multiple :initform options.~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c multiple-documentation-options-not-permitted)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "A slot can not have multiple :documentation options.~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c multiple-allocation-options-not-permitted)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "A slot can not have multiple :allocation options.~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c multiple-type-options-not-permitted)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "A slot can not have multiple :type options.~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c slot-documentation-option-must-be-string)
			     stream
			     (language (eql 'en-us)))
  (format stream
	  "The :documentation option of a slot must have a string argument, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c class-option-must-be-non-empty-list)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "A class option must be a a non-empty list, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c class-option-name-must-be-symbol)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "A class option name must be a symbol, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-documentation-option)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "A documentation option must have the form~@
           (:documentation <name>), but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-metaclass-option)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "A documentation option must have the form~@
           (:documentation <name>), but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c malformed-default-initargs-option)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "The DEFAULT-INITARG option takes the form~@
           (:default-initargs <name> <value> <name> <value>...), but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c default-initargs-option-once)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "The default-initargs option can appear only once in the~@
           list of class options, but a second such option:~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c documentation-option-once)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "The documentation option can appear only once in the~@
           list of class options, but a second such option:~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c metaclass-option-once)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "The metaclass option can appear only once in the~@
           list of class options, but a second such option:~@
           ~s was found."
	  (type-error-datum c)))

(defmethod report-condition ((c unknown-class-option)
			     stream
			     (langauge (eql 'en-us)))
  (format stream
	  "A class option is either ~@
           :default-initargs, :documentation, or :metaclass, but~@
           ~s was found."
	  (type-error-datum c)))
