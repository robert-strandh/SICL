(cl:in-package #:cleavir-code-utilities)

(defmethod acclimation:report-condition
    ((condition form-must-be-proper-list) stream (language acclimation:english))
  (format stream "A form must be a proper list,~@
                  but the following was found instead:~@
                  ~s" (form condition)))

(defmethod acclimation:report-condition
    ((condition invalid-number-of-arguments)
     stream
     (language acclimation:english))
  (format stream
          "Invalid number of arguments.~@
           There must be at least ~d arguments.~%"
          (min-argcount condition))
  (unless (null (max-argcount condition))
    (format stream
            "And there must be at most ~d arguments.~%"
            (max-argcount condition)))
  (format stream
          "The form that causes the problem was:~%~s"
          (form condition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Lambda list conditions.

(defmethod acclimation:report-condition
    ((c lambda-list-must-be-list) stream (language acclimation:english))
  (format stream
          "A lambda list must be a list.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c lambda-list-must-not-be-circular) stream (language acclimation:english))
  (format stream
          "A lambda list must not be a circular list.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c lambda-list-must-be-proper-list) stream (language acclimation:english))
  (format stream
          "This lambda list must be a proper list.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c lambda-list-keyword-not-allowed) stream (language acclimation:english))
  (format stream
          "Lambda list keyword ~s not allowed in this type of lambda list:~@
           ~s"
          (lambda-list-keyword c)
          (code c)))

(defmethod acclimation:report-condition
    ((c suspect-lambda-list-keyword) stream (language acclimation:english))
  (format stream
          "Suspect lambda list keyword ~s will be treated as an ordinary symbol.~@
           In this lambda list:~@
           ~s"
          (lambda-list-keyword c)
          (code c)))

(defmethod acclimation:report-condition
    ((c lambda-list-keyword-not-allowed-in-dotted-lambda-list)
     stream
     (language acclimation:english))
  (format stream
          "Lambda list keyword ~s not allowed in a dotted lambda list:~@
           ~s"
          (lambda-list-keyword c)
          (code c)))

(defmethod acclimation:report-condition
    ((c multiple-occurrences-of-lambda-list-keyword) stream (language acclimation:english))
  (format stream
          "Lambda list keyword ~s appears multiple times in lambda list:~@
           ~s"
          (lambda-list-keyword c)
          (code c)))

(defmethod acclimation:report-condition
    ((c incorrect-keyword-order) stream (language acclimation:english))
  (format stream
          "Incorrect lambda list keyword order.~@
           The keyword ~s incorrectly appears before the keyword ~s in:~@
           ~s"
          (lambda-list-keyword1 c)
          (lambda-list-keyword2 c)
          (code c)))

(defmethod acclimation:report-condition
    ((c both-rest-and-body-occur-in-lambda-list) stream (language acclimation:english))
  (format stream
          "Both &rest and &body may not occur in a lambda list.
           But they do in this lambda list:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c rest/body-must-be-followed-by-variable) stream (language acclimation:english))
  (format stream
          "The lambda list keyword &rest or &body must be followed by a variable.~@
           But this is not the case in this lambda list:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c atomic-lambda-list-tail-must-be-variable) stream (language acclimation:english))
  (format stream
          "The atomic tail of a lambda list must be a variable.~@
           But this is not the case in this lambda list:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c whole-must-be-followed-by-variable) stream (language acclimation:english))
  (format stream
          "The lambda list keyword &whole must be followed by a variable.~@
           But this is not the case in this lambda list:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c whole-must-appear-first) stream (language acclimation:english))
  (format stream
          "If &whole is used in a lambda list, it must appear first.~@
           But this is not the case in this lambda list:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c whole-must-be-followed-by-variable) stream (language acclimation:english))
  (format stream
          "The lambda list keyword &whole must be followed by a variable.~@
           But this is not the case in this lambda list:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c environment-must-be-followed-by-variable) stream (language acclimation:english))
  (format stream
          "The lambda list keyword &environment must be followed by a variable.~@
           But this is not the case in this lambda list:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c environment-can-appear-at-most-once) stream (language acclimation:english))
  (format stream
          "The lambda list keyword &environment can occur at most once in a lambda list.~@
           But it occurs several times in this lambda list:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c malformed-specialized-required) stream (language acclimation:english))
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

(defmethod acclimation:report-condition
    ((c malformed-ordinary-optional) stream (language acclimation:english))
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

(defmethod acclimation:report-condition
    ((c malformed-defgeneric-optional) stream (language acclimation:english))
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

(defmethod acclimation:report-condition
    ((c malformed-destructuring-optional) stream (language acclimation:english))
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

(defmethod acclimation:report-condition
    ((c malformed-ordinary-key) stream (language acclimation:english))
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

(defmethod acclimation:report-condition
    ((c malformed-defgeneric-key) stream (language acclimation:english))
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

(defmethod acclimation:report-condition
    ((c malformed-destructuring-key) stream (language acclimation:english))
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

(defmethod acclimation:report-condition
    ((c malformed-aux) stream (language acclimation:english))
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

(defmethod acclimation:report-condition
    ((c malformed-destructuring-tree) stream (language acclimation:english))
  (format stream
          "A destructuring tree can only contain CONS cells and~@
           symbols that are also not names of contstants.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c malformed-lambda-list-pattern) stream (language acclimation:english))
  (format stream
          "A lambda-list pattern must be either a tree containing only~@
           CONS cells and symbols that are also not names of contstants,~@
           or a list containing lambda-list keywords.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c required-must-be-variable) stream (language acclimation:english))
  (format stream
          "In this type of lambda list, the required parameter must~@
           be a variable which is also not the name of a constant.~@
           But the following was found instead:~@
           ~s"
          (code c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Conditions for forms with BODY.

(defmethod acclimation:report-condition
    ((c ordinary-body-must-be-proper-list) stream (language acclimation:english))
  (format stream
          "The BODY of a form such as LET, LET*, FLET, LABELS,~@
           or LOCALLY, must be a proper list.
           But the following was found instead:~@
           ~s"
          (body c)))

(defmethod acclimation:report-condition
    ((c function-body-must-be-proper-list) stream (language acclimation:english))
  (format stream
          "The BODY of a function must be a proper list.
           But the following was found instead:~@
           ~s"
          (body c)))
