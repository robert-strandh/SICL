(cl:in-package #:sicl-additional-conditions)

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

(defmethod acclimation:report-condition
    ((c sicl-type-error) stream (language acclimation:english))
  (format stream
          "Expected ~a.~@
           But got the following instead:~@
           ~s"
          (interpret-type (type-error-expected-type c))
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c both-test-and-test-not-given) stream (language acclimation:english))
  (format stream
          "Both keyword arguments :test and :test-not were given."))

(defmethod acclimation:report-condition
    ((c warn-both-test-and-test-not-given) stream (language acclimation:english))
  (format stream
          "Both keyword arguments :test and :test-not were given."))

(defmethod acclimation:report-condition
    ((c sicl-unbound-variable) stream (language acclimation:english))
  (format stream
          "The variable named ~s in unbound."
          (cell-error-name c)))

(defmethod acclimation:report-condition
    ((c sicl-undefined-function) stream (language acclimation:english))
  (format stream
          "The funcation named ~s in undefined."
          (cell-error-name c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile time conditions.

(defmethod acclimation:report-condition
    ((c multiple-documentation-strings-in-body) stream (language acclimation:english))
  (format stream
          "Multiple documentation strings found in code body:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c documentation-string-not-allowed-in-body) stream (language acclimation:english))
  (format stream
          "A documentation string was found where none is allowed:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c declaration-follows-form-in-body) stream (language acclimation:english))
  (format stream
          "Declarations can not follow the forms in a code body:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c form-too-short) stream (language acclimation:english))
  (format stream
          "The form:~@
           ~s~@
           should have at least ~a subforms, but has only ~a."
          (code c)
          (min-length c)
          (length (code c))))

(defmethod acclimation:report-condition
    ((c form-too-long) stream (language acclimation:english))
  (format stream
          "The form:~@
           ~s~@
           should have at most ~a subforms, but has ~a."
          (code c)
          (max-length c)
          (length (code c))))

(defmethod acclimation:report-condition
    ((c unknown-eval-when-situation) stream (language acclimation:english))
  (format stream
          "Unknown evaluation situation given:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c setq-must-have-even-number-arguments) stream (language acclimation:english))
  (format stream
          "An even number of arguments are required.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c setq-variable-must-be-symbol) stream (language acclimation:english))
  (format stream
          "A variable assigned to must be a symbol.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c tagbody-element-must-be-symbol-integer-or-compound-form)
     stream
     (language acclimation:english))
  (format stream
          "Element must be a symbol, an integer, or a compound form.~@
           But the following was found instead:~@
           ~s"
          (code c)))

(defmethod acclimation:report-condition
    ((c empty-body) stream (language acclimation:english))
  (format stream
          "The body of this form is empty:~@
           ~s"
          (code c)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Argument mismatch conditions.

(defmethod acclimation:report-condition
    ((c unrecognized-keyword-argument) stream (language acclimation:english))
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
