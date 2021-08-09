(cl:in-package #:sicl-iteration)

;;; A portable implementation of the Common Lisp
;;; iteration macros.
;;;
;;; This implementation does not use any iteration construct, nor any
;;; operations on sequences (other than the ones we define ourself
;;; here).  Implementations can therefore load this file very early on
;;; in the bootstrap process.  It allows for operations on sequences
;;; and the loop macro to be defined in terms of the macros defined
;;; here.

(defun do-dostar-expander
    (name let-type setq-type variable-clauses end-test body)
  ;; Do some syntax checking.
  (check-variable-clauses name variable-clauses)
  (body-must-be-proper-list name body)
  (unless (and (cleavir-code-utilities:proper-list-p end-test)
               (not (null end-test)))
    (error 'malformed-end-test
           :name name
           :found end-test))
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body body)
    (let ((start-tag (gensym)))
      `(block nil
         (,let-type ,(extract-bindings variable-clauses)
                    ,@declarations
                    (tagbody
                       ,start-tag
                       (when ,(car end-test)
                         (return
                           (progn ,@(cdr end-test))))
                       ,@forms
                       (,setq-type ,@(extract-updates
                                        variable-clauses))
                       (go ,start-tag)))))))
