(cl:in-package #:cleavir-cst-to-ast-test)

(defun ast-equal-p (x y)
  (cond ((and (typep x 'cleavir-ast:ast)
              (typep y 'cleavir-ast:ast))
         (and (eq (class-of x) (class-of y))
              (loop for save-info in (cleavir-io:save-info x)
                    always (ast-equal-p (funcall (cadr save-info) x)
                                        (funcall (cadr save-info) y)))))
        ((or (typep x 'cleavir-ast:ast)
             (typep y 'cleavir-ast:ast))
         nil)
        ((and (listp x)
              (listp y))
         (and (= (length x) (length y))
              (loop for element1 in x
                    for element2 in y
                    always (ast-equal-p element1 element2))))
        ((and (symbolp x)
              (symbolp y))
         (string= (symbol-name x) (symbol-name y)))
        (t
         (equal x y))))
