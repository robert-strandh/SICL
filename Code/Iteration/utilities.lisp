(cl:in-package #:sicl-iteration)

;;; Check that the binding var is a symbol.
(defun binding-var-must-be-symbol (name binding-var)
  (unless (symbolp binding-var)
    (error 'malformed-binding-var
           :name name
           :datum binding-var)))

;;; Check that the list-form is a list
;;; FIXME: signal a warning if list-form is not a proper-list
(defun list-form-must-be-list (name list-form)
  (unless (or (listp list-form) (symbolp list-form))
    (error 'malformed-list-form
           :name name
           :datum list-form)))

;;; Check that the count form is a positive integer.
(defun count-form-must-be-nonnegative-integer (name count-form)
  (unless (or (and (numberp count-form)
                   (not (minusp count-form)))
              (not (constantp count-form)))
    (error 'malformed-count-form
           :name name
           :datum count-form)))

;;; Check that iteration body is a proper list.
(defun body-must-be-proper-list (name body)
  (unless (cleavir-code-utilities:proper-list-p body)
    (error 'malformed-body
           :name name
           :datum body)))

(defun check-variable-clauses (name variable-clauses)
  (unless (cleavir-code-utilities:proper-list-p variable-clauses)
    (error 'malformed-variable-clauses
           :name
           :datum variable-clauses))
  (mapcar
   (lambda (clause)
     (unless (or (symbolp clause)
                 (and (consp clause)
                      (symbolp (car clause))
                      (or (null (cdr clause))
                          (null (cddr clause))
                          (null (cdddr clause)))))
       (error 'malformed-variable-clause
              :name name
              :found clause)))
   variable-clauses))

(defun extract-bindings (variable-clauses)
  (mapcar
   (lambda (clause)
     (cond ((symbolp clause) clause)
           ((null (cdr clause)) (car clause))
           (t (list (car clause) (cadr clause)))))
   variable-clauses))

(defun extract-updates (variable-clauses)
  (if (null variable-clauses) '()
      (let ((clause (car variable-clauses)))
        (if (and (consp clause)
                 (not (null (cddr clause))))
            (list* (car clause)
                   (caddr clause)
                   (extract-updates (cdr variable-clauses)))
            (extract-updates (cdr variable-clauses))))))
