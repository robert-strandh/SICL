(cl:in-package #:cleavir-ast-to-source)

(defgeneric to-source (ast dictionary))

(defun ast-to-source (ast)
  (to-source ast nil))

(defmethod to-source (ast dictionary)
  (declare (ignore ast dictionary))
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
