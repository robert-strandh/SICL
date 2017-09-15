(cl:in-package #:cleavir-cst-to-ast)

;;; We convert a LET form CST by transforming it into an equivalent
;;; LAMBDA form CST.

(defmethod convert-let (cst environment system)
  (cst:db origin (let-cst bindings-cst . body-forms-cst) cst
    (declare (ignore let-cst))
    (let* ((binding-csts (cst:listify bindings-cst))
           (variable-csts (loop for binding-cst in binding-csts
                                collect (if (cst:atom binding-cst)
                                            binding-cst
                                            (cst:first binding-cst))))
           (initform-csts (loop for binding-cst in binding-csts
                                collect (if (cst:atom binding-cst)
                                            (cst:cst-from-expression 'nil)
                                            (cst:second binding-cst))))
           (lambda-form-cst
             (make-instance 'cst:cons-cst
               :source origin
               :first (cst:cons (cst:cst-from-expression 'lambda)
                                (cst:cons (cst:cstify variable-csts)
                                          body-forms-cst))
               :rest (cst:cstify initform-csts))))
      (convert lambda-form-cst environment system))))
