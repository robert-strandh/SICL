(cl:in-package :sicl-cons)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Externally visible conditions

;;; This condition is used by functions and macros that require
;;; some argument to be a nonnegative integer.
(define-condition must-be-nonnegative-integer (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A nonnegative integer was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type '(integer 0)))

;;; This condition is used by functions and macros that require
;;; some argument to be a cons cell.
(define-condition must-be-cons (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A cons cell was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type 'cons))

;;; This condition is used by functions and macros that require
;;; some argument to be a list (a cons or nil).
(define-condition must-be-list (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A list (a cons or nil) was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some argument to be a property list.
(define-condition must-be-plist (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A property list was required, ~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type 'plist))

;;; This condition is used by functions and macros that require
;;; some list to be a proper list.
(define-condition must-be-proper-list (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A proper list was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some list to be either a proper list or a circular list.
(define-condition must-be-proper-or-circular-list (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A proper or circular list was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require some
;;; list to be either a proper list or a dotted list (but not a
;;; circular list).
(define-condition must-be-proper-or-dotted-list (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A proper or dotted list was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some argument to be a propterty list.
(define-condition must-be-property-list (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A property list was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions and macros that require
;;; some argument to be a association list.
(define-condition must-be-association-list (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A association list was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type 'list))

;;; This condition is used by functions that take :test and :test-not
;;; keyword arguments, and is signaled when both of those are given.
(define-condition both-test-and-test-not-given (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "Both keyword arguments :test and :test-not were given."))))

;;; This condition is used by the map* family functions when no lists
;;; were given, since those functions require at least one list
;;; argument.
(define-condition at-least-one-list-required (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "At least one argument is required,~@
                      but none was given."))))

;;; This condition is used by the list* function when no arguments
;;; were given, since that function requires at least one argument.
(define-condition at-least-one-argument-required (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "At least one argument is required~@
                      but no arguments were given."))))

;;; This condition is used by the pairlis function when
;;; the two lists are not of the same length.
(define-condition lists-must-have-the-same-length (error)
  ((%list1 :initarg :list1 :reader list1)
   (%list2 :initarg :list2 :reader list2))
  (:report (lambda (condition stream)
             (format stream
                     "The two lists passed as arguments must~@
                      have the same length, but the following~@
                      was given:~@
                      ~s~@
                      and~@
                      ~s."
                     (list1 condition)
                     (list2 condition)))))

;;; This condition is used by the function (SETF NTH)
;;; When NTHCDR of the original list is not a CONS cell.
(define-condition setf-nth-must-be-cons (must-be-cons)
  ((%original-list :initarg :original-list :reader original-list)
   (%cons-cell-count :initarg :cons-cell-count :reader cons-cell-count))
  (:report (lambda (condition stream)
             (format stream
                     "The ~:R CDR of the argument ~s~@
                      must be a CONS cell, but the following was given instead:~@
                      ~s."
                     (cons-cell-count condition)
                     (original-list condition)
                     (type-error-datum condition)))))

;;; This condition is used by macros that detect that there
;;; is both a :TEST and a :TEST-NOT, and that detection is
;;; done at macro-expansion time.
(define-condition warn-both-test-and-test-not-given (warning)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "both keyword arguments :TEST and :TEST-NOT~@
                      were given."))))
