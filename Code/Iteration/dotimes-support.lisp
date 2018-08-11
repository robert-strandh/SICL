(cl:in-package #:sicl-iteration)

;;; Macro DOTIMES.
;;;
;;; The spec says we have a choice between binding or assigning the
;;; variable in each iteration.  We have chosen to bind the variable
;;; once for the entire loop body.

(defun dotimes-expander (var count-form result-form body)
  ;; do some syntax checking
  (binding-var-must-be-symbol 'dotimes var)
  (count-form-must-be-nonnegative-integer 'dotimes count-form)
  (body-must-be-proper-list 'dotimes body)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body body)
    (let ((start-tag (gensym))
          (end-tag (gensym))
          (count-var (gensym)))
      `(let ((,count-var ,count-form)
             (,var 0))
         (declare (type unsigned-byte ,var))
         ,@declarations
         (block nil
           (tagbody
              ,start-tag
              (when (= ,var ,count-var)
                (go ,end-tag))
              (tagbody ,@forms)
              (incf ,var)
              (go ,start-tag)
              ,end-tag)
           ,result-form)))))
