(cl:in-package #:sicl-ast-evaluator)

(defvar *run-time-environment-name*)

(defgeneric translate-ast (client ast lexical-environment))

(defmethod translate-ast
    (client (ast ast:fdefinition-ast) lexical-environment)
  (let* ((name (ast:value (ast:name-ast ast)))
         (pair (assoc name *function-cells* :test #'equal)))
    (when (null pair)
      (setf pair (cons name (gensym)))
      (push pair *function-cells*))
    `(car ,(cdr pair))))

(defmethod translate-ast
    (client (ast ast:constant-ast) lexical-environment)
  `',(ast:value ast))

(defmethod translate-ast
    (client (ast ast:lexical-ast) lexical-environment)
  (find-identifier lexical-environment ast))

(defmethod translate-ast
    (client (ast ast:call-ast) lexical-environment)
  (let ((arguments-var (gensym)))
    (if (null (cleavir-cst-to-ast:origin ast))
        `(funcall
          ,(translate-ast client (ast:callee-ast ast) lexical-environment)
          ,@(loop for argument-ast in (ast:argument-asts ast)
                  collect (translate-ast
                           client argument-ast lexical-environment)))
        `(let* ((,arguments-var
                  (list ,@(loop for argument-ast in (ast:argument-asts ast)
                                collect (translate-ast
                                         client
                                         argument-ast
                                         lexical-environment))))
                (sicl-hir-evaluator:*call-stack*
                  (cons (make-instance 'sicl-hir-evaluator:call-stack-entry
                          :origin ',(cleavir-cst-to-ast:origin ast)
                          :arguments ,arguments-var)
                        sicl-hir-evaluator:*call-stack*)))
           (apply
            ,(translate-ast client (ast:callee-ast ast) lexical-environment)
            ,arguments-var)))))

(defmethod translate-ast
    (client (ast ast:function-ast) lexical-environment)
  (let* ((lambda-list (ast:lambda-list ast))
         (new-environment (augment-environment lexical-environment lambda-list))
         (body (translate-ast client (ast:body-ast ast) new-environment))
         (vars
           (remove-duplicates
            (loop for name being each hash-value of (first new-environment)
                  collect name))))
    `(lambda
         ,(loop for item in (ast:lambda-list ast)
                collect
                (cond ((member item lambda-list-keywords)
                       item)
                      ((atom item)
                       (find-identifier new-environment item))
                      ((= (length item) 2)
                       ;; It is an optional parameter.
                       `(,(find-identifier new-environment (first item))
                         nil
                         ,(find-identifier new-environment (second item))))
                      (t
                       ;; It is a keyword parameter.
                       `((,(first item)
                          ,(find-identifier new-environment (second item)))
                         nil
                         ,(find-identifier new-environment (third item))))))
       (let ,vars
         (declare (ignorable ,@vars))
         ,body))))

(defmethod translate-ast
    (client (ast ast:progn-ast) lexical-environment)
  `(progn ,@(loop for form-ast in (ast:form-asts ast)
                  collect (translate-ast
                           client form-ast lexical-environment))))

(defmethod translate-ast
    (client (ast ast:block-ast) lexical-environment)
  (add-identifier lexical-environment ast)
  (let ((name (find-identifier lexical-environment ast)))
    `(with-exit-point (,name)
       (block ,name
         ,(translate-ast
           client (ast:body-ast ast) lexical-environment)))))

(defmethod translate-ast
    (client (ast ast:return-from-ast) lexical-environment)
  (let ((name (find-identifier lexical-environment (ast:block-ast ast))))
    `(progn (unwind ',name)
            (return-from ,name
              ,(translate-ast
                client (ast:form-ast ast) lexical-environment)))))

(defmethod translate-ast
    (client (ast ast:tagbody-ast) lexical-environment)
  (let ((name (gensym)))
    (loop for item-ast in (ast:item-asts ast)
          when (typep item-ast 'ast:tag-ast)
            do (add-identifier lexical-environment item-ast name))
    `(with-exit-point (,name)
       (tagbody
          ,@(loop for item-ast in (ast:item-asts ast)
                  collect (if (typep item-ast 'ast:tag-ast)
                              (ast:name item-ast)
                              (translate-ast
                               client item-ast lexical-environment)))))))

(defmethod translate-ast
    (client (ast ast:go-ast) lexical-environment)
  (let* ((tag-ast (ast:tag-ast ast))
         (name (find-identifier lexical-environment tag-ast)))
    `(progn (unwind ',name)
            (go ,(ast:name tag-ast)))))

(defmethod translate-ast
    (client (ast ast:setq-ast) lexical-environment)
  `(setq ,(translate-ast client (ast:lhs-ast ast) lexical-environment)
         ,(translate-ast client (ast:value-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:unwind-protect-ast) lexical-environment)
  (let ((thunk-form
          (translate-ast
           client (ast:cleanup-thunk-ast ast) lexical-environment)))
  `(with-unwind-protect
       (,thunk-form)
     ,(translate-ast client (ast:protected-form-ast ast) lexical-environment)
     (funcall ,thunk-form))))

(defmethod translate-ast
    (client (ast ast:if-ast) lexical-environment)
  `(if ,(translate-ast client (ast:test-ast ast) lexical-environment)
       ,(translate-ast client (ast:then-ast ast) lexical-environment)
       ,(translate-ast client (ast:else-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:eq-ast) lexical-environment)
  `(eq ,(translate-ast client (ast:arg1-ast ast) lexical-environment)
       ,(translate-ast client (ast:arg2-ast ast) lexical-environment)))
