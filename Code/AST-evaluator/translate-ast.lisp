(cl:in-package #:sicl-ast-evaluator)

(defvar *run-time-environment-name*)

(defgeneric translate-ast (ast lexical-environment))

(defmethod translate-ast
    ((ast ast:fdefinition-ast) lexical-environment)
  `(env:fdefinition
    (sicl-environment:client ,*run-time-environment-name*)
    ,*run-time-environment-name*
    ,(translate-ast
      (ast:name-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:constant-ast) lexical-environment)
  `',(ast:value ast))

(defmethod translate-ast
    ((ast ast:lexical-ast) lexical-environment)
  (find-identifier lexical-environment ast))

(defmethod translate-ast
    ((ast ast:symbol-value-ast) lexical-environment)
  `(symbol-value
    ,(translate-ast
      (ast:name-ast ast) lexical-environment)
    ,*run-time-environment-name*))

(defmethod translate-ast
    ((ast ast:set-symbol-value-ast) lexical-environment)
  `(setf (symbol-value
          ,(translate-ast
            (ast:name-ast ast) lexical-environment)
          ,*run-time-environment-name*)
         ,(translate-ast
           (ast:value-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:call-ast) lexical-environment)
  (let ((arguments-var (gensym)))
    (if (null (cleavir-cst-to-ast:origin ast))
        `(funcall
          ,(translate-ast (ast:callee-ast ast) lexical-environment)
          ,@(loop for argument-ast in (ast:argument-asts ast)
                  collect (translate-ast
                           argument-ast lexical-environment)))
        `(let* ((,arguments-var
                  (list ,@(loop for argument-ast in (ast:argument-asts ast)
                                collect (translate-ast
                                         argument-ast
                                         lexical-environment))))
                (sicl-hir-evaluator:*call-stack*
                  (cons (make-instance 'sicl-hir-evaluator:call-stack-entry
                          :origin ',(cleavir-cst-to-ast:origin ast)
                          :arguments ,arguments-var)
                        sicl-hir-evaluator:*call-stack*)))
           (apply
            ,(translate-ast (ast:callee-ast ast) lexical-environment)
            ,arguments-var)))))

(defmethod translate-ast
    ((ast ast:function-ast) lexical-environment)
  (let* ((lambda-list (ast:lambda-list ast))
         (new-environment (augment-environment lexical-environment lambda-list))
         (body (translate-ast (ast:body-ast ast) new-environment))
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
    ((ast ast:progn-ast) lexical-environment)
  `(progn ,@(loop for form-ast in (ast:form-asts ast)
                  collect (translate-ast
                           form-ast lexical-environment))))

(defmethod translate-ast
    ((ast ast:block-ast) lexical-environment)
  (add-identifier lexical-environment ast)
  (let ((name (find-identifier lexical-environment ast)))
    `(with-exit-point (,name)
       (block ,name
         ,(translate-ast
           (ast:body-ast ast) lexical-environment)))))

(defmethod translate-ast
    ((ast ast:return-from-ast) lexical-environment)
  (let ((name (find-identifier lexical-environment (ast:block-ast ast))))
    `(progn (unwind ',name)
            (return-from ,name
              ,(translate-ast
                (ast:form-ast ast) lexical-environment)))))

(defmethod translate-ast
    ((ast ast:tagbody-ast) lexical-environment)
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
                               item-ast lexical-environment)))))))

(defmethod translate-ast
    ((ast ast:go-ast) lexical-environment)
  (let* ((tag-ast (ast:tag-ast ast))
         (name (find-identifier lexical-environment tag-ast)))
    `(progn (unwind ',name)
            (go ,(ast:name tag-ast)))))

(defmethod translate-ast
    ((ast ast:setq-ast) lexical-environment)
  `(setq ,(translate-ast (ast:lhs-ast ast) lexical-environment)
         ,(translate-ast (ast:value-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:unwind-protect-ast) lexical-environment)
  (let ((thunk-form
          (translate-ast
           (ast:cleanup-thunk-ast ast) lexical-environment)))
  `(with-unwind-protect
       (,thunk-form)
     ,(translate-ast (ast:protected-form-ast ast) lexical-environment)
     (funcall ,thunk-form))))

(defmethod translate-ast
    ((ast ast:if-ast) lexical-environment)
  `(if ,(translate-ast (ast:test-ast ast) lexical-environment)
       ,(translate-ast (ast:then-ast ast) lexical-environment)
       ,(translate-ast (ast:else-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:eq-ast) lexical-environment)
  `(eq ,(translate-ast (ast:arg1-ast ast) lexical-environment)
       ,(translate-ast (ast:arg2-ast ast) lexical-environment)))
