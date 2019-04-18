(cl:in-package #:cleavir-cst-to-ast)

;;; We need to build a CST corresponding to the following expression:
;;;
;;; `(cleavir-primop:call-with-variable-bound
;;;   ',variable
;;;   (cleavir-primop:ast ,value-ast)
;;;   (lambda () (cleavir-primop:ast ,next-ast)))
;;;
;;; The problem here is that we don't have VARIABLE, only the
;;; corresponding CST, and we do not want to lose the source
;;; information of the variable.  So we construct an ordinary Common
;;; Lisp list of CSTs, and then we convert that to a CST.
(defmethod convert-special-binding
    (variable-cst value-ast next-thunk env client)
  (let* ((call-with-variable-bound-cst
           (cst:cst-from-expression 'cleavir-primop:call-with-variable-bound))
         (quoted-variable-cst
           (cst:cstify `(,(cst:cst-from-expression 'quote)
                         ,variable-cst)))
         (new-cst (cst:cstify `(,call-with-variable-bound-cst
                                ,quoted-variable-cst
                                ,(cst:cst-from-expression
                                  `(cleavir-primop:ast ,value-ast))
                                ,(cst:cst-from-expression
                                  `(cleavir-primop:ast
                                    ,(let* ((cleavir-ast:*dynamic-environment*
                                              (make-instance 'cleavir-ast:lexical-ast
                                               :name '#:cwvb-dynamic-environment))
                                            (next-ast (funcall next-thunk)))
                                       (make-instance 'cleavir-ast:function-ast
                                         :body-ast next-ast
                                         :lambda-list nil
                                         :dynenv-out cleavir-ast:*dynamic-environment*))))))))
    (convert client new-cst env)))
