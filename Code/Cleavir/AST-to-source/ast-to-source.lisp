(cl:in-package #:cleavir-ast-to-source)

(defgeneric to-source (ast dictionary))

(defun ast-to-source (ast)
  (to-source ast nil))

(defun list-to-sources (list dictionary)
  (loop for elem in list collect (to-source elem dictionary)))

(defmethod to-source (ast dictionary)
  (warn "Don't know how to sourcify a ~a"
        (class-name (class-of ast)))
  `(??? ,@(list-to-sources (cleavir-ast:children ast)
                           dictionary)))

(defmethod to-source ((ast cleavir-ast:function-ast) dictionary)
  (let* ((entries '())
         (lambda-list
           (loop for item in (cleavir-ast:lambda-list ast)
                 collect (cond ((member item lambda-list-keywords)
                                item)
                               ((and (consp item)
                                     (= (length item) 2))
                                (let ((v1 (gensym))
                                      (v2 (gensym)))
                                  (push (cons (first item) v1) entries)
                                  (push (cons (second item) v2) entries)
                                  `(,v1 nil ,v2)))
                               ((and (consp item)
                                     (= (length item) 3))
                                (let ((v1 (gensym))
                                      (v2 (gensym)))
                                  (push (cons (second item) v1) entries)
                                  (push (cons (third item) v2) entries)
                                  `((,(first item) ,v1) nil ,v2)))
                               (t
                                (let ((v (gensym)))
                                  (push (cons item v) entries)
                                  v))))))
    `(lambda ,lambda-list
       ,(to-source (cleavir-ast:body-ast ast) (append entries dictionary)))))

(defmethod to-source ((ast cleavir-ast:setq-ast) dictionary)
  `(setq ,(to-source (cleavir-ast:lhs-ast ast) dictionary)
         ,(to-source (cleavir-ast:value-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:if-ast) dictionary)
  `(if ,(to-source (cleavir-ast:test-ast ast) dictionary)
       ,(to-source (cleavir-ast:then-ast ast) dictionary)
       ,(to-source (cleavir-ast:else-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:typeq-ast) dictionary)
  `(cleavir-primop:typeq
    ,(to-source (cleavir-ast:form-ast ast) dictionary)
    ,(to-source (cleavir-ast:type-specifier-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:literal-ast) dictionary)
  (declare (ignore dictionary))
  `',(cleavir-ast:value ast))

(defmethod to-source ((ast cleavir-ast:lexical-ast) dictionary)
  (or (cdr (assoc ast dictionary)) (cleavir-ast:name ast)))

(defmethod to-source ((ast cleavir-ast:call-ast) dictionary)
  `(funcall ,(to-source (cleavir-ast:callee-ast ast) dictionary)
            ,@(list-to-sources (cleavir-ast:argument-asts ast)
                               dictionary)))

(defmethod to-source ((ast cleavir-ast:progn-ast) dictionary)
  `(progn ,@(list-to-sources (cleavir-ast:children ast)
                             dictionary)))

(defmethod to-source ((ast cleavir-ast:multiple-value-call-ast)
                      dictionary)
  `(multiple-value-call
       ,(to-source (cleavir-ast:function-form-ast ast) dictionary)
     ,@(list-to-sources (cleavir-ast:form-asts ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:load-time-value-ast)
                      dictionary)
  `(load-time-value ,(cleavir-ast:form ast)
                    ,(cleavir-ast:read-only-p ast)))

(defmethod to-source ((ast cleavir-ast:fdefinition-ast) dictionary)
  `(fdefinition
    ,(to-source (cleavir-ast:name-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:eq-ast) dictionary)
  `(eq ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
       ,(to-source (cleavir-ast:arg2-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:symbol-value-ast)
                      dictionary)
  `(symbol-value
    ,(to-source (cleavir-ast:symbol-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:the-ast) dictionary)
  (let ((req (cleavir-ast:required-types ast))
        (opt (cleavir-ast:optional-types ast))
        (rest (cleavir-ast:rest-type ast)))
    (when opt (setf opt `(&optional ,@opt)))
    (when rest (setf rest `(&rest ,rest)))
    `(the (values ,@req ,@opt ,@rest)
          ,(to-source (cleavir-ast:form-ast ast) dictionary))))

(defmethod to-source ((ast cleavir-ast:go-ast) dictionary)
  `(go ,(cleavir-ast:name (cleavir-ast:tag-ast ast))))

(defmethod to-source ((ast cleavir-ast:block-ast) dictionary)
  (let ((name (gensym "BLOCK")))
    `(block ,name
       ,(to-source (cleavir-ast:body-ast ast)
                   (acons ast name dictionary)))))

(defmethod to-source ((ast cleavir-ast:return-from-ast)
                      dictionary)
  (let ((name (cdr (assoc (cleavir-ast:block-ast ast)
                          dictionary))))
    (unless name
      (error "BUG: Orphaned return-from ~a" ast))
    `(return-from ,name
       ,(to-source (cleavir-ast:form-ast ast) dictionary))))

(defmethod to-source ((ast cleavir-ast:tagbody-ast) dictionary)
  `(tagbody
      ,@(loop for item in (cleavir-ast:item-asts ast)
              when (typep item 'cleavir-ast:tag-ast)
                collect (cleavir-ast:name item)
              else collect (to-source item dictionary))))

(defmethod to-source ((ast cleavir-ast:values-ast) dictionary)
  `(cleavir-primop:values ,@(list-to-sources
                             (cleavir-ast:children ast)
                             dictionary)))
