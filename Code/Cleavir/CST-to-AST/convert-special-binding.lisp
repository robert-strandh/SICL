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
    (variable-cst value-ast next-thunk env system)
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
                                    ,(let* ((new-dynenv
                                              (cleavir-ast:make-dynamic-environment-ast
                                               '#:cwvb-dynamic-environment))
                                            (next-ast
                                              (let ((cleavir-ast:*dynamic-environment*
                                                      new-dynenv))
                                                (funcall next-thunk))))
                                       (cleavir-ast:make-function-ast
                                        next-ast nil new-dynenv))))))))
    (convert new-cst env system)))
