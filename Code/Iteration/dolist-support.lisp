(cl:in-package #:sicl-iteration)

;;; Macro DOLIST.
;;;
;;; The spec says that the variable is bound to nil when the
;;; result-form is evaluated.  But we don't want the declarations to
;;; have to include nil as one of the values of var.  For that reason,
;;; there needs to be a different binding of the variable when the
;;; forms of the body are evaluated and when the result-form is
;;; evaluated.
;;;
;;; The spec says we have a choice between binding or assigning the
;;; variable in each iteration.  For dolist, choosing assignment gets
;;; complicated in the first iteration though, because we would have
;;; to come up with an initial value of the variable that is
;;; compatible with the declarations.  For that reason, we choose to
;;; bind it.

(defun dolist-expander (var list-form result-form body)
  ;; Do some syntax checking.
  (binding-var-must-be-symbol 'dolist var)
  (list-form-must-be-list 'dolist list-form)
  (body-must-be-proper-list 'dolist body)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body body)
    (let ((start-tag (gensym))
          (end-tag (gensym))
          (list-var (gensym)))
      `(let ((,list-var ,list-form))
         (block nil
           (tagbody
              ,start-tag
              (when (endp ,list-var)
                (go ,end-tag))
              (let ((,var (car ,list-var)))
                ,@declarations
                (tagbody ,@forms))
              (pop ,list-var)
              (go ,start-tag)
              ,end-tag)
           (let ((,var nil))
             (declare (ignorable ,var))
             ,result-form))))))
