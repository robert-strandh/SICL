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
