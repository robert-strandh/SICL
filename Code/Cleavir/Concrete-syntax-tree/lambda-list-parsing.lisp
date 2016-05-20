(cl:in-package #:cleavir-cst)

;;;; We use the same convention as SMUG, i.e., a parser returns a list
;;;; of items, where each item is a CONS.  The CAR of the CONS is the
;;;; RESULT of the parse and the CDR of the CONS is the remaining
;;;; input.  Parse failure is indicated by an empty list of items
;;;; being returned.

;;; This parser is invoked only when INPUT is known not to be empty
;;; and the first element of INPUT is not a lambda-list keyword.  In
;;; other words, the first element of INPUT exists and it must be a
;;; CST representing a single symbol, in order for the parse to
;;; succeed.
(defun parse-required-parameter (input)
  (if (symbolp (expression (first input)))
      (list (make-instance 'required-parameter
	      :variable-cst (first input))
	    (rest input))
      '()))
