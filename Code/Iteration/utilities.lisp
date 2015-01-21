(cl:in-package #:sicl-iteration)

;;; Checks if the binding var is a symbol
(defun binding-var-must-be-symbol (name binding-var)
  (unless (symbolp binding-var)
    (error 'malformed-binding-var
           :name name
           :datum binding-var)))

;;; Checks if the list-form is a list
;;; FIXME: signal a warning if list-form is not a proper-list
(defun list-form-must-be-list (name list-form)
  (unless (or (listp list-form) (symbolp list-form))
    (error 'malformed-list-form
           :name name
           :datum list-form)))

;;; Checks if the count form is a positive integer
(defun count-form-must-be-nonnegative-integer (name count-form)
  (unless (or (and (numberp count-form)
                   (not (minusp count-form)))
              (not (constantp count-form)))
    (error 'malformed-count-form
           :name name
           :datum count-form)))

;;; Checks if iteration body is a proper list
(defun body-must-be-proper-list (name body)
  (unless (cleavir-code-utilities:proper-list-p body)
    (error 'malformed-body
           :name name
           :datum body)))

;;; For do and do* we need to map over the variable binding clauses.
;;; We therefore need mapcar or something similar.  But in order to
;;; avoid introducing a dependency on sequence operations, we define
;;; our own mapcar using only recursion.

(defun local-mapcar (function list)
  (if (null list)
      '()
      (cons (funcall function (car list))
	    (local-mapcar function (cdr list)))))

