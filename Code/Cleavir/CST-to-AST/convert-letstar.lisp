(cl:in-package #:cleavir-cst-to-ast)

;;; We convert a LET* form CST by transforming it into nested LET form
;;; CSTs and then converting those instead.  This is not trivial,
;;; because we need to associate the right declarations with the
;;; corresponding LET form CST.

(defmethod convert-let* (cst environment system)
  (cst:db origin (let*-cst bindings-cst . body-forms-cst) cst
    (declare (ignore let*-cst))
    (multiple-value-bind (declaration-csts body-csts)
        (cst:separate-ordinary-body body-forms-cst)
      (let* ((canonical-declaration-specifiers
              (cst:canonicalize-declarations system declaration-csts))
             (binding-csts (cst:listify bindings-cst))
             (variable-csts
               (loop for binding-cst in binding-csts
                     collect (if (cst:atom binding-cst)
                                 binding-cst
                                 (cst:first binding-cst)))))
        (multiple-value-bind (item-specific-dspecs remaining-dspecs)
            (itemize-declaration-specifiers (mapcar #'list variable-csts)
                                            canonical-declaration-specifiers)
          (loop with remaining-dspecs-cst = (cst:cstify remaining-dspecs)
                with result = (cst:cstify (cons remaining-dspecs-cst
                                                body-csts))
                for binding-cst in (reverse binding-csts)
                for declaration-cst in (reverse item-specific-dspecs)
                do (setf result
                         (cst:cons (cst:cst-from-expression 'let)
                                   (cst:cons binding-cst
                                             (if (null declaration-cst)
                                                 result
                                                 (cst:cons (car declaration-cst)
                                                           result)))))
                finally (return (convert result environment system))))))))
