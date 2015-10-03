(cl:in-package #:sicl-read)

(define-condition sicl-reader-error (reader-error cleavir-i18n:condition)
  ())

(define-condition unmatched-right-parenthesis (sicl-reader-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unmatched right parenthesis found."))))

;;; This condition is signaled when a token consisting
;;; only of (unescaped) dots was found.
(define-condition only-dots-in-token (sicl-reader-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "A token with only dots in it was found"))))

;;; This condition is signaled when a token consisting
;;; of a single (unescaped) dot was found.
(define-condition single-dot-token (only-dots-in-token)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "A token consisting of a single dot was found ~@
                     in a context that does not permit such a token."))))

(define-condition no-object-preceding-dot (sicl-reader-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "A left parenthesis cannot be ~
                     immediately followed by a dot"))))

(define-condition multiple-objects-following-dot (sicl-reader-error)
  ((%offending-expression
    :initarg :offending-expression
    :reader offending-expression))
  (:report
   (lambda (condition stream)
     (format stream "A second expression following a dot~@
                     inside a list was found: ~S."
	     (offending-expression condition)))))

(define-condition no-parameter-allowed (sicl-reader-error)
  ((%which-directive :initarg :which-directive :reader which-directive)
   (%parameter :initarg :parameter :reader parameter))
  (:report
   (lambda (condition stream)
     (format stream
	     "The ~a directive does not take a numeric parameter"
	     (which-directive condition)))))

(define-condition unknown-character-name (sicl-reader-error)
  ((%name :initarg :name :reader name)))

(define-condition invalid-character (sicl-reader-error)
  ((%char :initarg :char :reader char))
  (:report
   (lambda (condition stream)
     (format stream
	     "Invalid character ~s on stream ~s."
	     (char condition)
	     (stream-error-stream condition)))))
