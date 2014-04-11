(cl:in-package #:cleavir-ast-to-source)

(defgeneric to-source (ast dictionary))

(defun ast-to-source (ast)
  (to-source ast nil))

(defmethod to-source (ast dictionary)
  `(??? ,@(loop for child in (cleavir-ast:children ast)
		collect (to-source child dictionary))))

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
  `(setq ,(cdr (assoc (cleavir-ast:variable-ast ast) dictionary))
	 ,(to-source (cleavir-ast:value-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:if-ast) dictionary)
  `(if ,(to-source (cleavir-ast:test-ast ast) dictionary)
       ,(to-source (cleavir-ast:then-ast ast) dictionary)
       ,(to-source (cleavir-ast:else-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:typeq-ast) dictionary)
  `(typep ,(to-source (cleavir-ast:form-ast ast) dictionary)
	  ,(to-source (cleavir-ast:type-specifier-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:constant-ast) dictionary)
  (declare (ignore dictionary))
  `',(cleavir-ast:value ast))

(defmethod to-source ((ast cleavir-ast:lexical-ast) dictionary)
  (cdr (assoc ast dictionary)))

(defmethod to-source ((ast cleavir-ast:car-ast) dictionary)
  `(%car ,(to-source (cleavir-ast:cons-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:call-ast) dictionary)
  `(funcall ,(to-source (cleavir-ast:callee-ast ast) dictionary)
	    ,@(loop for arg in (cleavir-ast:argument-asts ast)
		    collect (to-source arg dictionary))))

(defmethod to-source ((ast cleavir-ast:global-ast) dictionary)
  `#',(cleavir-ast:name ast))
