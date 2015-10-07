(cl:in-package #:sicl-read)

(define-condition sicl-reader-error (reader-error cleavir-i18n:condition)
  ())

(define-condition unmatched-right-parenthesis (sicl-reader-error)
  ())

;;; This condition is signaled when a token consisting
;;; only of (unescaped) dots was found.
(define-condition only-dots-in-token (sicl-reader-error)
  ())

;;; This condition is signaled when a token consisting
;;; of a single (unescaped) dot was found.
(define-condition single-dot-token (only-dots-in-token)
  ())

(define-condition no-object-preceding-dot (sicl-reader-error)
  ())

(define-condition multiple-objects-following-dot (sicl-reader-error)
  ((%offending-expression
    :initarg :offending-expression
    :reader offending-expression)))

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
